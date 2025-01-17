unit RedeemerDataURI;

interface

uses
  Classes, Graphics;

type
  TRedeemerDataURI = class (TMemoryStream)
    constructor Create(const s: RawByteString); reintroduce;
    public
      function GetImageClassFromMIME(): TGraphicClass;
      var
        MIME: string;
        Encoding: RawByteString;
        IsBase64: Boolean; // könnte man später auch verstecken, denn das braucht eigentlich niemand
  end;

{$DEFINE GIF}
{$DEFINE JPEG}
{$DEFINE PNG}
{$DEFINE SVG}
//{$DEFINE BMP}
//{$DEFINE ICO}
//{$DEFINE META}

implementation

uses
    RTLConsts, SysUtils
  {$IF Defined(GIF)}
    , gifimg
  {$IFEND}
  {$IF Defined(JPEG)}
    , jpeg
  {$IFEND}
  {$IF Defined(PNG)}
    , pngimage
  {$IFEND}
  {$IF Defined(SVG)}
    , RedeemerSVG
  {$IFEND};

{ TRedeemerDataURI }

constructor TRedeemerDataURI.Create(const s: RawByteString);
var
  Header: RawByteString;
  HeaderEnd: Integer;
  CharsetStart: Integer;
  i: Integer;
  Length: Integer;
  Temp: Cardinal;
  Count: Integer;
  b: Byte;
function Base64Index(const b: Byte): Byte;
begin
  Result := Pos(Char(b), ('ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
                                           'abcdefghijklmnopqrstuvwxyz' +
                                           '0123456789+/')) - 1;
end;
function Read(): Byte;
function HexIndex(const b: Byte): Byte;
begin
  Result := Byte(Pos(AnsiString(Uppercase(Char(b))), RawByteString('0123456789ABCDEF')) - 1);
end;
function ReadChar(): Byte;
begin
  if i <= Length then
  Result := Byte(s[i])
  else
  Result := 0;
  Inc(i);
end;
begin
  Result := ReadChar();
  if Result = 37 then // %
  begin
    Result := HexIndex(ReadChar()) shl 4;
    Result := Result + HexIndex(ReadChar());
  end;
end;
begin
  inherited Create();

  // Gültigkeit des Headers überprüfen
  HeaderEnd := Pos(RawByteString(','), s);
  Header := Copy(s, 6, HeaderEnd - 6);
  if not SameText(string(Copy(s, 1, 5)), 'data:') or (HeaderEnd = 0) then
  raise EInvalidContainer.Create(SInvalidImage);


  // Header parsen: Ist Base64?
  if SameText(string(Copy(Header, System.Length(Header) - 6, 7)), ';base64') then
  begin
    IsBase64 := True;
    Header := Copy(Header, 1, System.Length(Header) - 7);
  end
  else
  IsBase64 := False;

  // Header parsen: Zeichensatz
  CharsetStart := Pos(AnsiString(';charset='), Header);
  if CharsetStart = 0 then
  Encoding := 'us-ascii'
  else
  begin
    Encoding := Copy(Header, CharsetStart + 9, High(Integer));
    Header := Copy(Header, 1, CharsetStart - 1);
  end;

  // Header parsen: MIME-Typ
  if Header = '' then
  MIME := 'text/plain'
  else
  MIME := string(Header);

  // Datenstrom in Bytes umsetzen
  Length := System.Length(s);
  Count := 0;
  Temp := 0;
  i := HeaderEnd+1;
  if IsBase64 then
  begin
    while i <= Length do
    begin
      b := Base64Index(Read());
      if b <> 255 then
      begin
        temp := temp shl 6 or b;
        inc(Count);
        if Count = 4 then
        begin
          temp := temp and $ff00 + temp shl 16 + temp shr 16; // Endianness drehen (vereinfacht weil nur 3 Byte verwendet)
          Write(temp, 3);
          temp := 0;
          Count := 0;
        end;
      end;
    end;
    if Count > 1 then // Rest behandeln, der kein 3er-Block ist (ein Base64-Rest kann nicht 1 lang sein)
    begin
      temp := temp shl (6 * (4 - Count));
      temp := temp and $ff00 + temp shl 16 + temp shr 16;
      Write(temp, Count - 1);
    end;
  end
  else
  while i <= Length do
  begin
    b := Read();
    Write(b, 1);
  end;
  Position := 0;
end;

function TRedeemerDataURI.GetImageClassFromMIME: TGraphicClass;
begin
  {$IF Defined(GIF)}
  if SameText(MIME, 'image/gif') then
  Result := TGIFImage
  else
  {$IFEND}
  {$IF Defined(JPEG)}
  if SameText(MIME, 'image/jpeg') then
  Result := TJPEGImage
  else
  {$IFEND}
  {$IF Defined(PNG)}
  if SameText(MIME, 'image/png') then
  Result := TPNGImage
  else
  {$IFEND}
  {$IF Defined(SVG)}
  if SameText(MIME, 'image/svg+xml') then
  Result := TSVGImage
  else
  {$IFEND}
  {$IF Defined(BMP)}
  if SameText(MIME, 'image/bmp') then
  Result := TBitmap
  else
  {$IFEND}
  {$IF Defined(ICO)}
  if SameText(MIME, 'image/vnd.microsoft.icon') then
  Result := TIcon
  else
  {$IFEND}
  {$IF Defined(META)}
  if SameText(MIME, 'image/emf') or
     SameText(MIME, 'image/wmf') or
     SameText(MIME, 'image/x-emf') or   // veraltet
     SameText(MIME, 'image/x-wmf') then // veraltet
  Result := TMetafile
  else
  {$IFEND}
  raise EInvalidImage.Create(SInvalidImage);
  //Result := nil;
end;

end.
