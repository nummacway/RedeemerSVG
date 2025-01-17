unit RedeemerGzip;

// Konvertiert einen GZIP-Stream in einen ZLIB-Stream
// RFC 1952

interface

uses
  Classes, SysUtils;

type TGzipOS = (osFAT         = 0,
                osAmiga       = 1,
                osVMS         = 2,
                osUnix        = 3,
                osVMCMS       = 4,
                osAtariTOS    = 5,
                osHPFS        = 6,
                osMac         = 7,
                osZ           = 8,
                osCPM         = 9,
                osTOPS20      = 10,
                osNTFS        = 11,
                osQDOS        = 12,
                osAcornRISCOS = 13,
                osUnknown     = 255);

type
  TRedeemerGzipAdapter = class
    public
      constructor CreateFromCompressed(From: TStream);
      constructor CreateFromUncompressed(From: TBytesStream);
      destructor Destroy(); override;
      procedure SaveToStream(Stream: TStream);
      procedure SaveToFile(FN: string);
      const
        ID1: Byte = 31; // Header
        ID2: Byte = 139; // Header
        CompressionMethod: Byte = 8; // Deflate
      var
        IsText, HasHeaderCRC, HasExtra, HasName, HasComment: Boolean;
        Timestamp: Cardinal;
        CompressionLevel: Byte; // 2 = max, 4 = so lala
        OS: TGzipOS;
        ExtraLen: Word;
        Extra: TBytes;
        Name, Comment: WideString;
        HeaderCRC: Word;
        Zlib: TMemoryStream;
        UncompressedCRC32: Cardinal;
        UncompressedSize: Cardinal;
  end;

function NowUTC(): TDateTime;

implementation

uses
  pngimage, zlib, DateUtils, Windows;

{ TRedeemerGzipAdapter }

constructor TRedeemerGzipAdapter.CreateFromCompressed(From: TStream);
function ReadByte: Byte;
begin
  From.Read(Result, 1);
end;
procedure ReadStr(out s: WideString);
var
  Temp: Byte;
begin
  s := '';
  repeat
    Temp := ReadByte;
    if Temp = 0 then
    Exit
    else
    s := s + WideChar(Temp); // Konvertiere ISO 8859-1 nach UTF-16
  until False;
end;
var
  Temp: Byte;
function ReadBool: Boolean;
begin
  Result := Temp mod 2 = 1;
  Temp := Temp shr 1;
end;
begin
  inherited Create;

  if (ReadByte <> ID1)
  or (ReadByte <> ID2)
  or (ReadByte <> CompressionMethod) then
  raise Exception.Create('Data error.');

  // Flags
  Temp := ReadByte;
  IsText := ReadBool;
  HasHeaderCRC := ReadBool;
  HasExtra := ReadBool;
  HasName := ReadBool;
  HasComment := ReadBool;

  // Sonstiger fixer Header
  From.Read(Timestamp, 4);
  CompressionLevel := ReadByte;
  From.Read(OS, 1);

  // Optionaler Header
  if HasExtra then
  begin
    From.Read(ExtraLen, 2);
    SetLength(Extra, ExtraLen);
    From.Read(Extra[0], ExtraLen);
  end;
  if HasName then
  ReadStr(Name);
  if HasComment then
  ReadStr(Comment);
  if HasHeaderCRC then
  From.Read(HeaderCRC, 2);

  Zlib := TMemoryStream.Create;
  try
    // Zlib-Header erstellen
    Temp := $78;
    Zlib.Write(Temp, 1);
    //if CompressionLevel = 4 then
    Temp := $9c; // tatsächlicher Level interessiert kein Schwein
    //else
    //Temp := $da;
    Zlib.Write(Temp, 1);

    // Zlib-Daten kopieren (ohne CRC32, denn das irritiert Zlib, da dessen CRC offenbar anders ist)
    Zlib.CopyFrom(From, From.Size - From.Position - 8);
    Zlib.Position := 0;

    From.Read(UncompressedCRC32, 4);
    From.Read(UncompressedSize, 4);
  except
    Zlib.Free;
    raise;
  end;
end;

constructor TRedeemerGzipAdapter.CreateFromUncompressed(From: TBytesStream);
var
  Compressor: TZCompressionStream;
  Temp: TMemoryStream;
begin
  UncompressedSize := From.Size;
  UncompressedCRC32 := not update_crc($ffffffff, @(From.Bytes[0]), UncompressedSize);
  Temp := TMemoryStream.Create();
  try
    Zlib := TMemoryStream.Create();
    Compressor := TZCompressionStream.Create(Temp);
    try
      From.Position := 0;
      Compressor.CopyFrom(From, UncompressedSize);
    finally
      Compressor.Free();
    end;
    // Header und CRC entfernen (irritiert GZip - ersteres verhindert die Nutzung von 7-Zip, letzteres führt zu einem CRC-Fehler)
    Temp.Position := 2;
    Zlib.CopyFrom(Temp, Temp.Size - 6);
  finally
    Temp.Free();
  end;
  Timestamp := DateTimeToUnix(NowUTC());
  CompressionLevel := 4; // das System würde 0x789C schreiben, das entspricht nicht-maximaler Kompression
end;

destructor TRedeemerGzipAdapter.Destroy;
begin
  inherited;
  Zlib.Free;
end;

procedure TRedeemerGzipAdapter.SaveToFile(FN: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FN, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free();
  end;
end;

procedure TRedeemerGzipAdapter.SaveToStream(Stream: TStream);
var
  Temp: Byte;
  Header: TBytesStream;
procedure WriteNullTerminatedString(const s: WideString);
var
  i: Integer;
begin
  for i := 1 to Length(s) do
  begin
    Temp := Byte(Ord(s[i]));
    Header.Write(Temp, 1);
  end;
  Temp := 0;
  Header.Write(Temp, 1);
end;
begin
  Header := TBytesStream.Create();
  try
    Header.Write(ID1, 1);
    Header.Write(ID2, 1);
    Header.Write(CompressionMethod, 1);
    Temp := 1 * Byte(IsText) +
            2 * Byte(HasHeaderCRC) +
            4 * Byte(HasExtra) +
            8 * Byte(HasName) +
           16 * Byte(HasComment);
    Header.Write(Temp, 1);
    Header.Write(Timestamp, 4);
    Header.Write(CompressionLevel, 1);
    Header.Write(OS, 1);
    if HasExtra then
    begin
      Header.Write(ExtraLen, 2);
      Header.Write(Extra[0], ExtraLen)
    end;
    if HasName then
    WriteNullTerminatedString(Name);
    if HasComment then
    WriteNullTerminatedString(Comment);
    if HasHeaderCRC then
    begin
      HeaderCRC := Word(not update_crc($ffffffff, @(Header.Bytes[0]), Header.Size));
      Header.Write(HeaderCRC, 2);
    end;
    Header.Position := 0;
    Stream.CopyFrom(Header, Header.Size);
    Zlib.Position := 0;
    Stream.CopyFrom(Zlib, Zlib.Size);
    Stream.Write(UncompressedCRC32, 4);
    Stream.Write(UncompressedSize, 4);
  finally
    Header.Free();
  end;
end;

function NowUTC(): TDateTime;
// https://www.delphipraxis.net/66519-lokale-zeit-und-utc.html
var
  SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
end;

end.
