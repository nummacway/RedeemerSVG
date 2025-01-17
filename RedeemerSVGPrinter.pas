unit RedeemerSVGPrinter;

interface

uses
  RedeemerSVG, Printers, Classes, RedeemerXML, Types;

type
  TRedeemerSVGPrinter = class helper for TPrinter
    private
      procedure PrintSVGFromString(XML: TRedeemerXML; const Area: TRect; const UseArea: Boolean); overload;
    public
      procedure PrintSVGFromString(const s: string); overload;
      procedure PrintSVGFromStream(Stream: TStream);
  end;

implementation

uses
  Math, pngimage, RedeemerAffineGeometry;

{ TRedeemerSVGPrinter }

procedure TRedeemerSVGPrinter.PrintSVGFromStream(Stream: TStream);
var
  Encoding: TCustomUTF8Encoding;
  sl: TStringList;
begin
  sl := TStringList.Create();
  Encoding := TCustomUTF8Encoding.Create;
  try
    sl.LoadFromStream(Stream, Encoding);
    PrintSVGFromString(sl.Text);
  finally
    sl.Free;
    Encoding.Free;
  end;
end;

procedure TRedeemerSVGPrinter.PrintSVGFromString(const s: string);
var
  XML: TRedeemerXML;
begin
  XML := TRedeemerXML.Create(s);
  try
    PrintSVGFromString(XML, Rect(0,0,0,0), False);
  finally
    XML.Free();
  end;
end;

procedure TRedeemerSVGPrinter.PrintSVGFromString(XML: TRedeemerXML; const Area: TRect; const UseArea: Boolean);
var
  Meet: Boolean;
  Align, Dimensions: TRealPoint;
  InitialViewbox: TRealRect;
  InitialTransformation: TAffineTransformation;
  Interpreter: TSVGInterpreter;
  DummyPNG: TPNGImage;
  ZoomFactor: Extended;
begin
  while XML.GoToAndGetNextTag do
  if (XML.CurrentTag = 'svg') or (XML.CurrentTag = 'pattern') then // pattern, da die Pattern-Tags wie eigene SVGs sind
  begin
    // Erstmal Größe lesen
    InitialViewbox := RealRect(0, 0, 300, 300);

    TSVGInterpreter.ReadAspectRatio(XML, Align, Meet);
    if TSVGInterpreter.ReadViewbox(XML, InitialViewbox) then
    TSVGInterpreter.ReadDimensions(XML, Dimensions, InitialViewbox)
    else
    begin
      TSVGInterpreter.ReadDimensions(XML, Dimensions, InitialViewbox);
      InitialViewbox.Width := Dimensions.x;
      InitialViewbox.Height := Dimensions.y;
    end;

    if UseArea then
    ZoomFactor := Min((Area.Right - Area.Left) / Dimensions.x, (Area.Bottom - Area.Top) / Dimensions.y)
    else
    ZoomFactor := Min(Self.PageWidth / Dimensions.x, Self.PageHeight / Dimensions.y);
    Dimensions.x := Trunc(Dimensions.x * ZoomFactor);
    Dimensions.y := Trunc(Dimensions.y * ZoomFactor);

    // Standard-Bosstransformation: Koordinatensystem umwandeln
    if UseArea then
    InitialTransformation := AffineTransformation(1, 0, 0, 1, Area.Left-0.5, Area.Top-0.5)
    else
    InitialTransformation := AffineTransformation(1, 0, 0, 1, -0.5, -0.5);

    // Transformation in den richtigen Zeichenbereich
    TSVGInterpreter.MakeViewportTransformation(InitialTransformation, InitialViewbox, Dimensions, Align, Meet); // keine Position im Wurzel-Tag

    DummyPNG := TPNGImage.CreateBlank(COLOR_RGB, 8, 1, 1);
    try
      Interpreter := TSVGInterpreter.Create(XML, Self.Canvas, DummyPNG.Canvas, InitialViewbox, InitialTransformation, 1, True, 1); // Alpha ist zwar nicht disabled, aber wir brauchen uns darum nicht kümmern
      Interpreter.Free();

      Exit; // nur erstes <svg> in der Wurzel bearbeiten
    finally
      DummyPNG.Free();
    end;
  end;
end;

end.
