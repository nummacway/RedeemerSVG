unit RedeemerSVG;

(* RedeemerSVG.TSVGInterpreter
 * 0.6-beta
 * Copyright © 2017 Janni K. (redeemer.biz)
 * Copyright © 2018-2021 Fensterbörse Verden GmbH (fenster24.de)
 *)

interface

uses
  PNGImage, Graphics, Sysutils, RedeemerAffineGeometry, RedeemerXML, Windows,
  RedeemerHypertextColors, RedeemerHypertextColorsCSS, Classes,
  StrUtils, Kollegah, Math, RedeemerScale, RedeemerSVGHelpers, Types,
  Generics.Collections, RedeemerInheritablePNG;

type
  TCustomUTF8Encoding = class(TUTF7Encoding)
  public
    constructor Create; override;
end;

type TRealRect = record
  Left, Top, Width, Height: Extended;
end;

type TFill = record
  Color: TColor;
  Opacity: Extended;
  Rule: Integer; // ALTERNATE, WINDING
  Pattern: string;
end;

type TStroke = record
  Width: Extended; // negative Angaben: Prozent, bedeutet Prozent/100*sqrt((Breite²+Höhe²)/2)
  VectorEffect: Boolean; // true = Width skaliert nicht
  Linecap: Cardinal;
  Linejoin: Cardinal;
  Opacity: Extended;
  Miterlimit: Extended;
  Color: TColor;
  DashArray: string;
end;

type TCSSFont = record
  Family: string;
  Style: Boolean; // Italic oder nicht
  Weight: Boolean; // Bold oder nicht
  Size: Extended;
  Underline: Boolean;
  LineThrough: Boolean;
end;

type TSVGContext = record
  Transformations: TAffineTransformation; // werden von außen nach innen berechnet (Assoziativgesetz gilt), die innerste interne Transformation ist eine eigene Variable der Klasse
  LastViewBox: TRealRect;
  Fill: TFill;
  Stroke: TStroke;
  Font: TCSSFont;
  Display: Boolean;
  Opacity: Extended;
  PaintOrderStroke: Boolean; // Kontur zuerst ja/nein
  TextAnchor: Integer;
end;

type TPatternDIBClass = class(TMemoryStream) // keine Records möglich, da auch gepackte Records mit gepackten Arrays nicht hintereinander im Speicher sind, wenn die Größe des RGB-Arrays unbekannt/dynamisch ist
  constructor Create(PNG: TPNGImage);
end;

type TSloppyTransparencyDIB = packed record
  Bitmapinfo: tagBITMAPINFOHEADER;
  Data: packed array[0..35] of Byte;
  constructor Create(Color: TColor; Transparency: Extended);
end;

type
  TSVGImage = class;
  TSVGMetafile = class;

  TSizeCallbackEvent = procedure (const Sender: TSVGImage; const Viewport: TRealRect; var Dimensions: TRealPoint) of object;
  TEMFSizeCallbackEvent = procedure (const Sender: TSVGMetafile; const Viewport: TRealRect; var Dimensions: TRealPoint) of object;

  TSVGInterpreter = class
    constructor Create(XML: TRedeemerXML; ChromaCanvas, OpacityCanvas: TCanvas; const InitialViewbox: TRealRect; const InitialTransformation: TAffineTransformation; const FinalSupersample: Byte; const AlphaDisabled: Boolean; const NonScalingStrokeFactor: Byte);
    public
      class procedure ReadDimensions(XML: TRedeemerXML; var Dimensions: TRealPoint; const LastViewBox: TRealRect);
      class function  ReadViewbox(XML: TRedeemerXML; var ViewBox: TRealRect): Boolean;
      class procedure ReadAspectRatio(XML: TRedeemerXML; out Align: TRealPoint; out Meet: Boolean);
      class procedure MakeViewportTransformation(var Target: TAffineTransformation; const ViewBox: TRealRect; const Dimensions: TRealPoint; const Align: TRealPoint; const Meet: Boolean);
    private
      procedure InitDrawing();
      procedure FinishDrawing(const Context: TSVGContext);
      function  GetProperty(const Name: string; const CanAttribute: Boolean; const CanCSS: Boolean; out Value: string): Boolean;
      function  GetOnlyValue(const Attribute: string; out Value: Extended): Boolean; overload;
      function  GetOnlyValue(const Attribute: string; out Value: Extended; const PercentageMeasure: Extended): Boolean; overload;
      function  GetOnlyValueDef(const Attribute: string; const Default: Extended): Extended; overload;
      function  GetOnlyValueDef(const Attribute: string; const PercentageMeasure: Extended; const Default: Extended): Extended; overload;
      function  GetURLRef(const URL: string; const List: TDictionary<string,Integer>; out Value: Integer): Boolean;
      function  GetColorExt(const S: string; out Color: TColor): Boolean;
      procedure LoadBrush(const Fill: TFill);
      procedure LoadPen(const Stroke: TStroke; const Context: TSVGContext);
      procedure LoadFont(const Font: TCSSFont);
      procedure DrawPoly(Context: TSVGContext; const d: string);
      procedure HandleTag(const Context: TSVGContext; const Visible: Boolean);
      procedure HandleGroup2(Context: TSVGContext; const FullSVG: Boolean = False); // svg, g
      procedure HandleCircle(Context: TSVGContext; const IsEllipse: Boolean);
      procedure HandleRect(Context: TSVGContext);
      procedure HandleLine(Context: TSVGContext);
      procedure HandleText(Context: TSVGContext);
      procedure HandleImage(Context: TSVGContext);
      procedure HandleUse(Context: TSVGContext);
      procedure HandleDefs();
      function  ReadPosition(): TRealPoint;
      function  ReadStyle(var Context: TSVGContext): Boolean;
      procedure ReadFont(var Font: TCSSFont);
      procedure ReadStroke(var Stroke: TStroke);
      procedure ReadFill(var Fill: TFill);
      var
        CurrentStyle: TStyle;
        OpacityCanvas, ChromaCanvas, TempCanvas: TCanvas;
        XML: TRedeemerXML;
        InnerTransformation: TAffineTransformation;
        Symbols, Colors: TDictionary<string,Integer>;
        Patterns: TDictionary<string,TPatternDIBClass>;
        Recalls: TList<string>;
        CurrentPenOpacitySloppyDIB, CurrentPenChromaSloppyDIB1{, CurrentPenChromaSloppyDIB2}: TSloppyTransparencyDIB;
        //CurrentBrushOpacitySloppyDIB, CurrentBrushChromaSloppyDIB1, CurrentBrushChromaSloppyDIB2: TSloppyTransparencyDIB;
        AlphaDisabled: Boolean;
        FinalSupersample: Byte;
        Supersample: Boolean;
        AlphaOverride: TColor;
        NonScalingStrokeFactor: Byte;
      const
        TempSupersample = 32;
  end;

  TSVGImage = class (TRedeemerInheritablePNG)
    constructor Create(); override;
    protected
      var
        Downsample: Boolean;
    public
      procedure LoadFromString(const s: string); overload;
      procedure LoadFromString(XML: TRedeemerXML); overload;
      procedure LoadFromStream(Stream: TStream); override;
      class procedure PushSetCallback(Event: TSizeCallbackEvent);
      class procedure PopUndoCallback();
      var
        Callback: TSizeCallbackEvent;
        Supersample: Boolean;
        AlphaOverride: TColor;
  end;

  TSVGMetafile = class (TMetafile)
    constructor Create(); override;
    strict private
      procedure FixRDPBug(const Handle: HDC);
    public
      procedure LoadFromString(const s: string); overload;
      procedure LoadFromString(XML: TRedeemerXML); overload;
      procedure LoadFromStream(Stream: TStream); override;
      var
        Callback: TEMFSizeCallbackEvent;
        NonScalingStrokeFactor: Byte;
        WordFix: Boolean; // Word verwendet das Koordinatensystem von SVG, daher sind Bilder um eine halbe Einheit nach oben links verschoben
  end;

function RealRect(const Left, Top, Width, Height: Extended): TRealRect;

var
  CallbackStack: array of TSizeCallbackEvent;
  SizeCallback: TSizeCallbackEvent = nil;

implementation

uses
  RedeemerDataURI,
  Forms; // benötigt für Screen.Fonts

function RealRect(const Left, Top, Width, Height: Extended): TRealRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Width := Width;
  Result.Height := Height;
end;

{ TSVGInterpreter }

constructor TSVGImage.Create;
begin
  inherited Create();
  Callback := SizeCallback;
  AlphaOverride := clNone;
  Supersample := True;
  Downsample := True;
end;

constructor TSVGInterpreter.Create(XML: TRedeemerXML; ChromaCanvas, OpacityCanvas: TCanvas; const InitialViewbox: TRealRect; const InitialTransformation: TAffineTransformation; const FinalSupersample: Byte; const AlphaDisabled: Boolean; const NonScalingStrokeFactor: Byte);
var
  Context: TSVGContext;
  StartPos: Integer;
  ID: string;
begin
  Self.XML := XML;
  Self.ChromaCanvas := ChromaCanvas;
  Self.OpacityCanvas := OpacityCanvas;
  Self.AlphaDisabled := AlphaDisabled;
  Self.NonScalingStrokeFactor := NonScalingStrokeFactor;
  // TempCanvas auf OpacityCanvas zu setzeb, verkleinert EMF-Exporte und behebt
  // Fehler mit Software wie Affinity, die mit AbortPath nicht kennen und vor
  // der Bosstransformation gezeichnete Dinge übernehmen, index nicht auf der
  // finalen Canvas zwischengezeichnet wird
  Self.TempCanvas := OpacityCanvas;
  InnerTransformation := AffineScale(1 / TempSupersample, 1 / TempSupersample);
  Colors := Generics.Collections.TDictionary<string,Integer>.Create();
  Symbols := Generics.Collections.TDictionary<string,Integer>.Create();
  Patterns := Generics.Collections.TObjectDictionary<string,TPatternDIBClass>.Create([doOwnsValues]);
  Recalls := Generics.Collections.TList<string>.Create();
  Self.FinalSupersample := FinalSupersample;
  try
    Context.LastViewBox := InitialViewbox;
    // Restlichen Kontext initialisieren
    Context.Fill.Rule := WINDING;
    Context.Fill.Opacity := 1;
    Context.Fill.Color := clBlack;
    Context.Stroke.Color := clNone;
    Context.Stroke.Width := 1;
    Context.Stroke.Linecap := PS_ENDCAP_FLAT;
    Context.Stroke.Linejoin := PS_JOIN_MITER;
    Context.Stroke.Opacity := 1;
    Context.Stroke.Miterlimit := 4;
    Context.Font.Family := 'Times New Roman';
    Context.Font.Size := 16;
    Context.Font.Weight := False;
    Context.Font.Style := False;
    Context.Font.Underline := False;
    Context.Font.LineThrough := False;
    Context.Opacity := 1;
    Context.Display := True;
    Context.PaintOrderStroke := False;
    Context.TextAnchor := TA_LEFT;
    // Standard-Bosstransformation: temporäres Supersampling rückgängig machen, Koordinatensystem umwandeln
    Context.Transformations := InitialTransformation;


    // Definitionen laden
    StartPos := XML.Position;
    while XML.GoToAndGetNextTag do
    if XML.GetAttribute('id', ID) then
    Symbols.Add(ID, XML.Position);

    // Reset
    XML.Position := StartPos;
    XML.Done := False;
    XML.LoadTagName();
    XML.LoadAttributes();
    HandleGroup2(Context);
  finally
    Symbols.Free();
    Colors.Free();
    Recalls.Free();
    Patterns.Free();
  end;
end;

procedure TSVGInterpreter.DrawPoly(Context: TSVGContext; const d: string);
var
  p: TPath;
  LastEndpoint, LastBezier, NextEndpoint, FirstBezier, SecondBezier, Radii, DerotatedMidway, DerotatedCenter, Center, Dummy2, LastStartpoint: TRealPoint;
  Dummy, Angle, Theta, DeltaTheta: Extended;
  SweepFlag, LargeArcFlag: Boolean;
function ConditionalRelativeX(const f: Extended): Extended;
begin
  if AnsiChar(p.LastType) in ['a'..'z'] then
  Result := LastEndpoint.x + f
  else
  Result := f;
end;
function ConditionalRelativeY(const f: Extended): Extended;
begin
  if AnsiChar(p.LastType) in ['a'..'z'] then
  Result := LastEndpoint.y + f
  else
  Result := f;
end;
procedure DrawBezier(const SecondPoint: TRealPoint);
var
  Points: packed array[0..2] of tagPOINT;
begin
  Points[0].X := Round(FirstBezier.x * TempSupersample);
  Points[0].Y := Round(FirstBezier.y * TempSupersample);
  Points[1].X := Round(SecondPoint.x * TempSupersample);
  Points[1].Y := Round(SecondPoint.y * TempSupersample);
  Points[2].X := Round(LastEndpoint.x * TempSupersample);
  Points[2].Y := Round(LastEndpoint.y * TempSupersample);
  PolyBezierTo(TempCanvas.Handle, Points, 3);
end;
procedure DrawLineToEndpoint();
begin
  LineTo(TempCanvas.Handle,
         Round(LastEndpoint.x*TempSupersample),
         Round(LastEndpoint.y*TempSupersample));
end;
procedure MakeAbsolute(out Target: TRealPoint; const Source: TRealPoint);
begin
  Target.x := ConditionalRelativeX(Source.x);
  Target.y := ConditionalRelativeY(Source.y);
end;
procedure ForceGetPoint(out Point: TRealPoint);
begin
  if not p.GetNextNumber(Point.x) then Abort;
  if not p.GetNextNumber(Point.y) then Abort;
end;
procedure DrawArc(const ThetaEnd: Extended);
function EllipsePoint(const Theta: Extended): TRealPoint;
begin
  Result.x := Center.x + Radii.x * Cos(Angle) * Cos(Theta) - Radii.y * Sin(Angle) * Sin(Theta);
  Result.y := Center.y + Radii.x * Sin(Angle) * Cos(Theta) + Radii.y * Cos(Angle) * Sin(Theta);
end;
function EllipseDerive(const Theta: Extended): TRealPoint;
begin
  Result.x := -Radii.x * Cos(Angle) * Sin(Theta) - Radii.y * Sin(Angle) * Cos(Theta);
  Result.y := -Radii.x * Sin(Angle) * Sin(Theta) + Radii.y * Cos(Angle) * Cos(Theta);
end;
var
  PositionToIntersect: Extended;
begin
  // Position der Kontrollpunkte auf dem Weg zwischen einem Punkt auf dem Kreis und dem Punkt, an dem sich die Tangenten der beiden Punkte kreuzen, berechnen
  PositionToIntersect := sin(ThetaEnd - Theta) * (sqrt(4 + 3*sqr(tan((ThetaEnd - Theta) / 2))) - 1) / 3;
  FirstBezier := EllipsePoint(Theta);
  with EllipseDerive(Theta) do
  begin
    FirstBezier.x := FirstBezier.x + PositionToIntersect * x;
    FirstBezier.y := FirstBezier.y + PositionToIntersect * y;
  end;
  LastEndpoint := EllipsePoint(ThetaEnd);
  with EllipseDerive(ThetaEnd) do
  begin
    SecondBezier.x := LastEndpoint.x - PositionToIntersect * x;
    SecondBezier.y := LastEndpoint.y - PositionToIntersect * y;
  end;
  DrawBezier(SecondBezier);
end;
begin
  p := TPath.Create(d);
  LastEndpoint := RealPoint(0,0);
  LastStartpoint := LastEndpoint; // nicht offiziell, aber irgendeinen Startwert muss ich sowieso setzen
  try
    if not ReadStyle(Context) then Exit;
    InitDrawing;
    try
      while p.GetNextType() <> #4 do
      case Uppercase(p.LastType)[1] of
        // LineTo
        'L': begin
               ForceGetPoint(NextEndpoint);
               MakeAbsolute(LastEndpoint, NextEndpoint);
               DrawLineToEndpoint();
             end;
        // MoveTo
        'M': begin
               ForceGetPoint(NextEndpoint);
               MakeAbsolute(LastEndpoint, NextEndpoint);
               LastStartpoint := LastEndpoint;
               MoveToEx(TempCanvas.Handle,
                 Round(LastEndpoint.x*TempSupersample),
                 Round(LastEndpoint.y*TempSupersample),
                 nil);
             end;
        // Horizontal Line To
        'H': begin
               if not p.GetNextNumber(NextEndpoint.x) then Abort;
               LastEndpoint.x := ConditionalRelativeX(NextEndpoint.x);
               DrawLineToEndpoint();
             end;
        // Vertical Line To
        'V': begin
               if not p.GetNextNumber(NextEndpoint.y) then Abort;
               LastEndpoint.y := ConditionalRelativeY(NextEndpoint.y);
               DrawLineToEndpoint();
             end;
        // ClosePath
        'Z': begin
               LastEndpoint := LastStartpoint;
               CloseFigure(TempCanvas.Handle);
             end;
        // CubicBézierCurveTo
        'C', 'S': begin
               if UpperCase(p.LastType) = 'S' then
               begin
                 if (UpperCase(p.SecondToLastType) = 'C') or (UpperCase(p.SecondToLastType) = 'S') then
                 begin
                   // Punktspiegelung
                   FirstBezier.x := 2 * LastEndpoint.x - LastBezier.x;
                   FirstBezier.y := 2 * LastEndpoint.y - LastBezier.y;
                 end
                 else
                 FirstBezier := LastEndpoint;
               end
               else
               begin
                 ForceGetPoint(FirstBezier);
                 MakeAbsolute(FirstBezier, FirstBezier);
               end;
               ForceGetPoint(LastBezier);
               ForceGetPoint(NextEndpoint);
               MakeAbsolute(LastBezier, LastBezier);
               MakeAbsolute(LastEndpoint, NextEndpoint);
               DrawBezier(LastBezier);
             end;
        // QuadraticBézierCurveTo
        'Q', 'T': begin
               if UpperCase(p.LastType) = 'T' then
               begin
                 if (UpperCase(p.SecondToLastType) = 'Q') or (UpperCase(p.SecondToLastType) = 'T') then
                 begin
                   // Punktspiegelung
                   LastBezier.x := 2 * LastEndpoint.x - LastBezier.x;
                   LastBezier.y := 2 * LastEndpoint.y - LastBezier.y;
                 end
                 else // ergibt null Sinn, denn wir zeichnen einfach eine Gerade
                 FirstBezier := LastEndpoint;
               end
               else
               begin
                 ForceGetPoint(FirstBezier);
                 MakeAbsolute(LastBezier, FirstBezier);
               end;
               ForceGetPoint(NextEndpoint);
               // Umwandeln von quadratischer Kurve in kubische Kurve (laut deutscher Wikipedia)
               FirstBezier.x := LastEndpoint.x + 2 * (LastBezier.x - LastEndpoint.x) / 3;
               FirstBezier.y := LastEndpoint.y + 2 * (LastBezier.y - LastEndpoint.y) / 3;
               MakeAbsolute(LastEndpoint, NextEndpoint);
               SecondBezier.x := LastEndpoint.x + 2 * (LastBezier.x - LastEndpoint.x) / 3;
               SecondBezier.y := LastEndpoint.y + 2 * (LastBezier.y - LastEndpoint.y) / 3;
               DrawBezier(SecondBezier);
             end;
        // ArcTo
        'A': begin
               ForceGetPoint(Radii);
               if not p.GetNextNumber(Angle) then Abort;
               Angle := FloatPositiveModulo(Angle, 360); // Definition: Winkel mod 360
               if not p.GetNextFlag(Dummy) then Abort;
               LargeArcFlag := Dummy <> 0; // Definition: alles außer 0 ist 1
               if not p.GetNextFlag(Dummy) then Abort;
               SweepFlag := Dummy <> 0;    // Definition: alles außer 0 ist 1
               ForceGetPoint(NextEndpoint);
               MakeAbsolute(NextEndpoint, NextEndpoint);
               if (LastEndpoint.x = NextEndpoint.x) and (LastEndpoint.y = NextEndpoint.y) then
               Continue;

               // Implementation nach https://www.w3.org/TR/SVG/implnote.html#ArcImplementationNotes F.6.5
               // und https://mortoray.com/2017/02/16/rendering-an-svg-elliptical-arc-as-bezier-curves/
               DerotatedMidway := AffineTransformation(AffineRotation(-Angle),
                                  RealPoint((LastEndpoint.x - NextEndpoint.x)/2, (LastEndpoint.y - NextEndpoint.y)/2));
               Dummy := sqrt((DerotatedMidway.x*DerotatedMidway.x)/(Radii.x*Radii.x)+
                             (DerotatedMidway.y*DerotatedMidway.y)/(Radii.y*Radii.y));
               if Dummy > 1 then // Radius zu klein
               begin
                 Radii.x := Dummy * Radii.x;
                 Radii.y := Dummy * Radii.y;
               end;
               Dummy := radii.x*radii.x*DerotatedMidway.y*DerotatedMidway.y+radii.y*radii.y*DerotatedMidway.x*DerotatedMidway.x;
               Dummy := (radii.x*radii.x*radii.y*radii.y-Dummy)/Dummy;
               if Dummy > 0 then // Rundungsfehler korrigieren
               Dummy := sqrt(Dummy)
               else
               Dummy := 0;
               if SweepFlag = LargeArcFlag then
               Dummy := -Dummy;
               DerotatedCenter.x := Dummy*radii.x*DerotatedMidway.y/radii.y;
               DerotatedCenter.y := -Dummy*radii.y*DerotatedMidway.x/radii.x;
               Center := AffineTransformation(AffineRotation(Angle), DerotatedCenter);
               Center.x := Center.x + (LastEndpoint.x + NextEndpoint.x)/2;
               Center.y := Center.y + (LastEndpoint.y + NextEndpoint.y)/2;

               Dummy2 := RealPoint((DerotatedMidway.x-DerotatedCenter.x)/Radii.x, (DerotatedMidway.y-DerotatedCenter.y)/Radii.y);
               Theta := RadAngle(RealPoint(1,0), Dummy2);
               DeltaTheta := FloatPositiveModulo(RadAngle(Dummy2,RealPoint((-DerotatedMidway.x-DerotatedCenter.x)/Radii.x, (-DerotatedMidway.y-DerotatedCenter.y)/Radii.y)), Pi * 2);
               if not SweepFlag then
               DeltaTheta := DeltaTheta - 2 * Pi;
               Dummy := 2 * Byte(SweepFlag) - 1; // Signum von DeltaTheta

               Angle := Angle / 180 * pi; // Ab jetzt Bogenmaß
               while Dummy * DeltaTheta > Pi/2 do
               begin
                 DrawArc(Theta+Dummy*Pi/2);
                 DeltaTheta := DeltaTheta - Dummy * Pi/2; // rechne Richtung 0
                 Theta := Theta + Dummy * Pi/2;
               end;
               DrawArc(Theta+DeltaTheta);

               LastEndpoint := NextEndpoint;
               DrawLineToEndpoint();
             end;
        // CentripetalCatmullRomTo (wird zu einer Geraden)
        'R': begin
               while p.GetNextNumber(Dummy) do // so lange Punkte laden, bis es nicht mehr geht, dann zum vorletzten(!) Punkt eine Linie zeichnen
               begin
                 NextEndpoint.x := NextEndpoint.y; // speichern der letzten vier Koordinaten, damit wir am Ende auf den vorletzten Punkt zugreifen können
                 NextEndpoint.y := LastEndPoint.x;
                 LastEndPoint.x := LastEndpoint.y;
                 LastEndPoint.y := Dummy;
               end;
               MakeAbsolute(LastEndpoint, NextEndpoint);
               DrawLineToEndpoint();
             end;
        // Bearing (wird ignoriert)
        'B': if not p.GetNextNumber(Dummy) then Abort;
      end;
      FinishDrawing(Context);
    except
      AbortPath(TempCanvas.Handle);
      raise Exception.Create('DEBUG: <path> failed at ' + IntToStr(p.Position) + ' on input:' + #13#10 + d);
    end;
  finally
    p.Free;
  end;
end;

procedure TSVGInterpreter.FinishDrawing(const Context: TSVGContext);
begin
  LoadBrush(Context.Fill);
  LoadPen(Context.Stroke, Context);
  EndPath(TempCanvas.Handle);
  if AlphaDisabled then
  Kollegah.Bosstransformation(TempCanvas.Handle, [ChromaCanvas.Handle], AffineTransformation(InnerTransformation, Context.Transformations), Context.PaintOrderStroke)
  else
  Kollegah.Bosstransformation(TempCanvas.Handle, [ChromaCanvas.Handle, OpacityCanvas.Handle], AffineTransformation(InnerTransformation, Context.Transformations), Context.PaintOrderStroke);
end;

function TSVGInterpreter.GetColorExt(const S: string; out Color: TColor): Boolean;
var
  i: Integer;
begin
  if StartsText('url(#', s) then // Farbreferenz
  begin
    Result := GetURLRef(S, Colors, i);
    if Result then
    Color := TColor(i);
  end
  else
  Result := HTMLToColor(S, Color, CSSColors);
end;

function TSVGInterpreter.GetOnlyValue(const Attribute: string; out Value: Extended): Boolean;
var
  s: string;
begin
  Result := XML.GetAttribute(Attribute, s);
  if Result then
  Result := TCoordinates.GetOnlyValue(s, Value);
end;

function TSVGInterpreter.GetOnlyValue(const Attribute: string; out Value: Extended; const PercentageMeasure: Extended): Boolean;
var
  s: string;
begin
  Result := XML.GetAttribute(Attribute, s);
  if Result then
  Result := TCoordinates.GetOnlyValue(s, Value, PercentageMeasure);
end;

function TSVGInterpreter.GetOnlyValueDef(const Attribute: string; const Default: Extended): Extended;
var
  s: string;
  b: Boolean;
begin
  b := XML.GetAttribute(Attribute, s);
  if b then
  Result := TCoordinates.GetOnlyValueDef(s, Default)
  else
  Result := Default;
end;

function TSVGInterpreter.GetOnlyValueDef(const Attribute: string; const PercentageMeasure: Extended; const Default: Extended): Extended;
var
  s: string;
  b: Boolean;
begin
  b := XML.GetAttribute(Attribute, s);
  if b then
  Result := TCoordinates.GetOnlyValueDef(s, PercentageMeasure, Default)
  else
  Result := Default;
end;

function TSVGInterpreter.GetProperty(const Name: string; const CanAttribute, CanCSS: Boolean; out Value: string): Boolean;
begin
  Result := False;
  if CanCSS then
  Result := CurrentStyle.GetProperty(Name, Value);
  if not Result and CanAttribute then
  Result := XML.GetAttribute(Name, Value);
end;

function TSVGInterpreter.GetURLRef(const URL: string; const List: TDictionary<string,Integer>; out Value: Integer): Boolean;
var
  s, s2: string;
begin
  Result := False;
  if List = Colors then
  begin
    if not TStyleSplitter.GetBracket(URL, s, s2) then Exit;
    if not SameText(s, 'url') then Exit;
  end
  else
  s2 := URL;

  if StartsStr('#', s2) then
  begin
    Delete(s2, 1, 1);
    Result := List.ContainsKey(s2);
    if Result then
    Value := List[s2];
  end;
end;

procedure TSVGInterpreter.HandleCircle(Context: TSVGContext; const IsEllipse: Boolean);
var
  rx, ry, cx, cy: Extended;
begin
  if not ReadStyle(Context) then Exit;

  // Koordinaten laden
  if IsEllipse then
  begin
    if not GetOnlyValue('rx',rx,Context.LastViewBox.Width) then Exit;
    if not GetOnlyValue('ry',ry,Context.LastViewBox.Height) then Exit;
  end
  else
  begin
    if not GetOnlyValue('r',rx,(sqrt((sqr(Context.LastViewBox.Width) + sqr(Context.LastViewBox.Height)) / 2))) then Exit;
    ry := rx;
  end;

  begin
    InitDrawing;
    cx := GetOnlyValueDef('cx',Context.LastViewBox.Width,0);
    cy := GetOnlyValueDef('cy',Context.LastViewBox.Height,0);
    Ellipse(TempCanvas.Handle,
      Integer(Round((cx-rx)*TempSupersample)),
      Integer(Round((cy-ry)*TempSupersample)),
      Integer(Round((cx+rx)*TempSupersample)),
      Integer(Round((cy+ry)*TempSupersample))
    );
    FinishDrawing(Context);
  end;
end;

procedure TSVGInterpreter.HandleDefs();
var
  NextStopName: string;
  i: TColor;
  SVG: TSVGImage;
begin
  if XML.IsSelfClosing then Exit; // Inkscape, was machst du für einen Blödsinn?
  NextStopName := '';
  while XML.GoToAndGetNextTag do
  begin
    // Gruppen müssen bearbeitet werden, damit die Sichtbarkeit korrekt für alle Unterelement gilt, Rest nicht
    if XML.CurrentTag = '/defs' then
    Exit
    else
    if XML.CurrentTag = 'symbol' then
    Symbols.AddOrSetValue(XML.GetAttribute('id'), XML.Position)
    else
    if XML.CurrentTag = 'pattern' then
    begin
      NextStopName := XML.GetAttribute('id');
      XML.Position := XML.Position - 1;
      SVG := TSVGImage.Create();
      try
        SVG.AlphaOverride := Self.AlphaOverride;
        SVG.Supersample := Self.Supersample;
        SVG.Downsample := False;
        SVG.LoadFromString(XML);
        Patterns.Add(NextStopName, TPatternDIBClass.Create(SVG));
      finally
        SVG.Free();
      end;
    end
    else
    if (XML.CurrentTag = 'radialgradient') or (XML.CurrentTag = 'lineargradient') then
    NextStopName := XML.GetAttribute('id')
    else
    if (XML.CurrentTag = 'stop') and (NextStopName <> '') then
    if RedeemerHypertextColors.HTMLToColor(XML.GetAttribute('stop-color'), i, CSSColors) then
    begin
      Colors.Add(NextStopName, Integer(i));
      NextStopName := '';
    end
    else
    else
    if XML.CurrentTag = 'solidcolor' then
    if RedeemerHypertextColors.HTMLToColor(XML.GetAttribute('solid-color'), i, CSSColors) then
    begin
      Colors.Add(XML.GetAttribute('id'), Integer(i));
      NextStopName := '';
    end;
  end;
end;

procedure TSVGInterpreter.HandleGroup2(Context: TSVGContext; const FullSVG: Boolean = False);
var
  EndTag: string;
  Dimensions, Align: TRealPoint;
  Visible, Meet: Boolean;
begin
  EndTag := '/' + XML.CurrentTag;
  Visible := ReadStyle(Context);
  if FullSVG then
  begin
    ReadDimensions(XML, Dimensions, Context.LastViewBox);
    ReadAspectRatio(XML, Align, Meet);

    with ReadPosition do
    Context.Transformations := AffineTransformation(AffineTranslation(x,y), Context.Transformations);

    if ReadViewbox(XML, Context.LastViewBox) then
    MakeViewportTransformation(Context.Transformations, Context.LastViewBox, Dimensions, Align, Meet)
    else
    Context.LastViewBox := RealRect(0, 0, Dimensions.x, Dimensions.y);
  end;
  while XML.GoToAndGetNextTag do
  begin
    if XML.CurrentTag = EndTag then
    Exit
    else
    HandleTag(Context, Visible);
  end;
end;

procedure TSVGInterpreter.HandleImage(Context: TSVGContext);
var
  p1, p2: TRealPoint;
  s: string;
  Data: TRedeemerDataURI;
  img: TGraphic;
  Transformation: TAffineTransformation;
  p2i: TPoint;
  r: TRect;

  Align: TRealPoint;
  Meet: Boolean;
begin
  if not ReadStyle(Context) then Exit;

  //p1 := ReadPosition();
  ReadDimensions(XML, p2, Context.LastViewBox);
  ReadAspectRatio(XML, Align, Meet);
  if XML.GetAttribute('xlink:href', s) then
  if StartsStr('data:', s) then
  begin

    // TODO: Kann man eventuell mal try..except drum rum machen
    Data := TRedeemerDataURI.Create(AnsiString(s));
    try
        // Bild laden
        img := Data.GetImageClassFromMIME().Create();
        try
        img.LoadFromStream(Data);

        // Viewport erstellen
        with ReadPosition do
        Context.Transformations := AffineTransformation(AffineTranslation(x,y), Context.Transformations);

        MakeViewportTransformation(Context.Transformations, RealRect(0,0,img.Width,img.Height), p2, Align, Meet);
        // Weitere Punkte laden
        // (prinzipiell könnte man unten einfach die InnerTransformation weglassen,
        // die einfach nur die Multiplikation mit TempSupersample rückgängig macht,
        // aber es soll ja ordentlich sein)
        //p2.x := (p2.x + p1.x) * TempSupersample;
        //p2.y := (p2.y + p1.y) * TempSupersample;
        //p1.x := p1.x * TempSupersample;
        //p1.y := p1.y * TempSupersample;
        p1 := RealPoint(0,0);
        p2.x := img.Width * TempSupersample;
        p2.y := img.Height * TempSupersample;


        // Punkte hier bereits transformieren
        // RedeemerSVG unterstützt für Image-Tags kein Drehen (außer um 180°) und keine Scherungen!
        Transformation := AffineTransformation(InnerTransformation, Context.Transformations);
        with AffineTransformation(Transformation, p2) do
        p2i := Point(Round(X), Round(Y));
        with AffineTransformation(Transformation, p1) do
        r := Rect(Round(X), Round(Y), p2i.X, p2i.Y);
        OpacityCanvas.Brush.Style := bsSolid; // Soll sichtbar sein (Opacity kleiner Schwellenwert wird bei ReadStyle geprüft)

        OpacityCanvas.FillRect(r);
        ChromaCanvas.StretchDraw(r, img);
      finally
        img.Free();
      end;
    finally
      Data.Free();
    end;
  end;
end;

procedure TSVGInterpreter.HandleTag(const Context: TSVGContext; const Visible: Boolean);
var
  d: string;
begin
  // Diese Methode entscheidet je nach Tag, welche Behandlungsroutine aufgerufen werden muss
  // Gruppen müssen immer bearbeitet werden, damit jedes Element einmal bearbeitet wird
  if (XML.CurrentTag = 'g') or (XML.CurrentTag = 'symbol') then
  HandleGroup2(Context)
  else
  if XML.CurrentTag = 'defs' then
  HandleDefs()
  else
  if XML.CurrentTag = 'use' then
  HandleUse(Context)
  else
  if XML.CurrentTag = 'svg' then
  HandleGroup2(Context, True)
  else
  // falls gezeichnet werden muss, bearbeite sichtbare Objekte, die keine (zu iterierenden) Kinder haben werden
  if Visible then
  if XML.CurrentTag = 'rect' then
  HandleRect(Context)
  else
  if XML.CurrentTag = 'circle' then
  HandleCircle(Context, False)
  else
  if XML.CurrentTag = 'ellipse' then
  HandleCircle(Context, True)
  else
  if XML.CurrentTag = 'line' then
  HandleLine(Context)
  else
  if XML.CurrentTag = 'polyline' then
  begin
    if XML.GetAttribute('points', d) then
    DrawPoly(Context, 'M' + d);
  end
  else
  if XML.CurrentTag = 'polygon' then
  begin
    if XML.GetAttribute('points', d) then
    DrawPoly(Context, 'M' + d + 'Z');
  end
  else
  if XML.CurrentTag = 'path' then
  begin
    if XML.GetAttribute(string('d'), d) then // wird ohne die sinnlose Typumwandlung manchmal aus irgendwelchen Gründen angekreidet
    DrawPoly(Context, d);
  end
  else
  if XML.CurrentTag = 'text' then
  HandleText(Context)
  else
  if XML.CurrentTag = 'image' then
  HandleImage(Context);
end;

procedure TSVGInterpreter.HandleLine(Context: TSVGContext);
begin
  if not ReadStyle(Context) then Exit;
  InitDrawing;
  MoveToEx(TempCanvas.Handle,
    Round(GetOnlyValueDef('x1',Context.LastViewBox.Width,0)*TempSupersample),
    Round(GetOnlyValueDef('y1',Context.LastViewBox.Height,0)*TempSupersample),
    nil);
  LineTo(TempCanvas.Handle,
    Round(GetOnlyValueDef('x2',Context.LastViewBox.Width,0)*TempSupersample),
    Round(GetOnlyValueDef('y2',Context.LastViewBox.Height,0)*TempSupersample));
  FinishDrawing(Context);
end;

procedure TSVGInterpreter.HandleRect(Context: TSVGContext);
var
  x,y,h,w,rx,ry: Extended;
begin
  if not ReadStyle(Context) then Exit;
  if GetOnlyValue('width',w,Context.LastViewBox.Width) then
  if GetOnlyValue('height',h,Context.LastViewBox.Height) then
  begin
    InitDrawing;
    x := GetOnlyValueDef('x',Context.LastViewBox.Width,0);
    y := GetOnlyValueDef('y',Context.LastViewBox.Height,0);
    // Wird nur ein Rundungs-Wert angegeben, erhält der andere dessen Wert - inklusive der Bezugsnorm für Prozentangaben!
    if GetOnlyValue('rx',rx,Context.LastViewBox.Width) then
    ry := GetOnlyValueDef('ry',Context.LastViewBox.Height,rx)
    else
    begin
      ry := GetOnlyValueDef('ry',Context.LastViewBox.Height,0);
      rx := ry;
    end;

    RoundRect(TempCanvas.Handle,
      Integer(Round(x*TempSupersample)),
      Integer(Round(y*TempSupersample)),
      Integer(Round((x+w)*TempSupersample)),
      Integer(Round((y+h)*TempSupersample)),
      Integer(Round(rx*2*TempSupersample)), // GDI benutzt den Durchmesser, SVG den Radius
      Integer(Round(ry*2*TempSupersample))
    );
    FinishDrawing(Context);
  end;
end;

procedure TSVGInterpreter.HandleText(Context: TSVGContext);
var
  x,y: TCoordinates;
  x2,y2: Extended;
  s,s2: string;
  i, count: Integer;
begin
  if not ReadStyle(Context) then Exit;

  // Attribute/Eigenschaften laden
  x := TCoordinates.Create(XML.GetAttributeDef('x','0'));
  y := TCoordinates.Create(XML.GetAttributeDef('y','0'));
  try
    SetTextAlign(TempCanvas.Handle, Context.TextAnchor or TA_BASELINE);
    // Text zeichnen
    s := XML.GetInnerTextAndSkip;
    if s <> '' then
    begin
      InitDrawing();
      LoadFont(Context.Font);

      // Zählen, in wievielen Einzelteilen der Text gerendet werden muss
      count := 0;
      while x.GetNextCoordinate(1, x2) do // konkreter Wert für Prozentwert derzeit egal
      inc(count);
      x.Position := 1; // Position des Koordinaten-Splitters fürs tatsächliche Rendern zurücksetzen

      // Einzelteile rendern
      i := 0;
      while x.GetNextCoordinate(Context.LastViewBox.Width, x2) do
      begin
        inc(i);
        if i < count then
        begin
        s2 := Copy(s, 1, 1); // soll nur verhindern, über das Ende hinaus zu lesen, daher keine Verwendung von s[1]
        Delete(s, 1, 1);
        end
        else
        s2 := s;
        y.GetNextCoordinate(Context.LastViewBox.Height, y2); // wenn nicht, dann halt nicht (bleibt unverändert)
        ExtTextOut(TempCanvas.Handle, Integer(Round(x2*TempSupersample)), Integer(Round(y2*TempSupersample)), 0, nil, PChar(s2), Length(s2), nil);
      end;
      FinishDrawing(Context);
    end;
  finally
    x.Free;
    y.Free;
  end;
end;

procedure TSVGInterpreter.HandleUse(Context: TSVGContext);
var
  OldPos, NewPos, StackIndex: Integer;
  s: string;
  Dimensions, Align, Position: TRealPoint;
  Meet: Boolean;
begin
  // Eigenschaften des aufrufenden use-Tags lesen
  ReadStyle(Context);

  OldPos := XML.Position;
  if XML.GetAttribute('xlink:href', s) then
  if GetURLRef(s, Symbols, NewPos) then
  if not Recalls.Contains(s) then // Endlosschleife verhindern
  try
    // Viewbox-relevante Daten aus use-Tag lesen
    ReadDimensions(XML, Dimensions, Context.LastViewBox); // wird verworfen, wenn es keine Viewbox gibt
    Position := ReadPosition;

    // Zu Definition springen
    XML.Position := NewPos;
    StackIndex := Recalls.Add(s);
    XML.LoadTagName();
    XML.LoadAttributes();

    // In jedem Fall kann aber x und y angewendet werden
    // Dies wird eigentlich VOR der Translation (oben in ReadStyle geladen) angewandt,
    // aber durch die Schachtelbarkeit von Elementen berechnet RedeemerSVG Transformationen von außen nach innen (Assoziativgesetz)
    Context.Transformations := AffineTransformation(AffineTranslation(Position.x,Position.y), Context.Transformations);

    // Tag der Definition bearbeiten
    if XML.CurrentTag = 'symbol' then
    begin
      // Viewbox-relevante Daten aus symbol-Tag lesen
      ReadAspectRatio(XML, Align, Meet);
      if ReadViewbox(XML, Context.LastViewBox) then
      MakeViewportTransformation(Context.Transformations, Context.LastViewBox, Dimensions, Align, Meet);
    end;

    HandleTag(Context, True);
    Recalls.Delete(StackIndex);
  finally
    XML.Position := OldPos;
    //XML.LoadAttributes und XML.LoadTagName nicht nötig, da auf Tag-Name und Attribute nicht mehr zugegriffen wird (übergeordnete Schleife springt sofort zum nächsten Tag)
  end;
end;

procedure TSVGInterpreter.InitDrawing;
begin
  SetBkMode(OpacityCanvas.Handle, Windows.TRANSPARENT);
  BeginPath(OpacityCanvas.Handle);
  SetBkMode(ChromaCanvas.Handle, Windows.TRANSPARENT);
  BeginPath(ChromaCanvas.Handle);
end;

procedure TSVGInterpreter.LoadFont(const Font: TCSSFont);
begin
  TempCanvas.Font.Height := -Round(Font.Size * TempSupersample);
  TempCanvas.Font.Name := Font.Family;
  TempCanvas.Font.Style := [];
  if Font.Weight then
  TempCanvas.Font.Style := {ChromaPNG.Canvas.Font.Style +} [fsBold];
  if Font.Style then
  TempCanvas.Font.Style := TempCanvas.Font.Style + [fsItalic];
  if Font.Underline then
  TempCanvas.Font.Style := TempCanvas.Font.Style + [fsUnderline];
  if Font.LineThrough then
  TempCanvas.Font.Style := TempCanvas.Font.Style + [fsStrikeOut];
end;

procedure TSVGImage.LoadFromStream(Stream: TStream);
var
  Encoding: TCustomUTF8Encoding;
  sl: TStringList;
begin
  sl := TStringList.Create;
  Encoding := TCustomUTF8Encoding.Create;
  try
    sl.LoadFromStream(Stream, Encoding);
    LoadFromString(sl.Text);
  finally
    sl.Free;
    Encoding.Free;
  end;
end;

procedure TSVGImage.LoadFromString(XML: TRedeemerXML);
var
  Meet: Boolean;
  Align, Dimensions: TRealPoint;
  TempPNG: TPNGImage;
  FinalSupersample: Byte;
  AlphaDisabled: Boolean;
  InitialViewbox: TRealRect;
  InitialTransformation: TAffineTransformation;
  Interpreter: TSVGInterpreter;
  OpacityPNG, ChromaPNG: TPNGImage;
begin
  AlphaDisabled := AlphaOverride <> clNone;
  while XML.GoToAndGetNextTag do
  if (XML.CurrentTag = 'svg') or (XML.CurrentTag = 'pattern') then // pattern, da die Pattern-Tags wie eigene SVGs sind
  begin
    // Erstmal Größe lesen
    InitialViewbox := RealRect(0, 0, 300, 300);
    //ReadPosition;
    TSVGInterpreter.ReadAspectRatio(XML, Align, Meet);
    if TSVGInterpreter.ReadViewbox(XML, InitialViewbox) then
    TSVGInterpreter.ReadDimensions(XML, Dimensions, InitialViewbox)
    else
    begin
      TSVGInterpreter.ReadDimensions(XML, Dimensions, InitialViewbox);
      InitialViewbox.Width := Dimensions.x;
      InitialViewbox.Height := Dimensions.y;
    end;
    //Context.LastViewBox := RealRect(0,0,Dimensions.x,Dimensions.y);

    // Größe vom Benutzer bestätigen lassen
    if Assigned(Callback) then
    Callback(Self, InitialViewbox, Dimensions);

    FinalSupersample := 1 or Byte(Supersample or not AlphaDisabled) shl 1;
    // Standard-Bosstransformation: temporäres Supersampling rückgängig machen, Koordinatensystem umwandeln
    InitialTransformation := AffineTransformation(FinalSupersample, 0, 0, FinalSupersample, -0.5, -0.5); // warum das -0,5 sein muss, kann man sich bei Koordinaten vorstellen, die auf ,5 enden.

    // Transformation in den richtigen Zeichenbereich
    TSVGInterpreter.MakeViewportTransformation(InitialTransformation, InitialViewbox, Dimensions, Align, Meet); // keine Position im Wurzel-Tag

    // Kein Supersampling => Wir zeichnen direkt
    if (FinalSupersample = 1) or not Downsample then
    begin
      InitBlankNonPaletteImage(COLOR_RGB, 8, Round(Dimensions.x * FinalSupersample), Round(Dimensions.y * FinalSupersample));
      OpacityPNG := TPngImage.CreateBlank(COLOR_GRAYSCALE, 8, 1, 1);
      ChromaPNG := Self;
      ChromaPNG.Canvas.Brush.Color := AlphaOverride;
      ChromaPNG.Canvas.FillRect(Rect(0, 0, ChromaPNG.Width, ChromaPNG.Height));
    end
    else
    // Zeichenflächen initialisieren (ursprünglicher Join-Algorithmus hatte RGB bei Opacity, war das für Rastergrafiken nötig?)
    begin
      ChromaPNG := TPngImage.CreateBlank(COLOR_RGB, 8, Round(Dimensions.x) * FinalSupersample, Round(Dimensions.y) * FinalSupersample);
      if AlphaDisabled then
      begin
        ChromaPNG.Canvas.Brush.Color := AlphaOverride;
        ChromaPNG.Canvas.FillRect(Rect(0, 0, ChromaPNG.Width, ChromaPNG.Height));
      end;
      if AlphaDisabled then
      OpacityPNG := TPngImage.CreateBlank(COLOR_GRAYSCALE, 8, 1, 1)
      else
      OpacityPNG := TPngImage.CreateBlank(COLOR_GRAYSCALE, 8, Round(Dimensions.x) * FinalSupersample, Round(Dimensions.y) * FinalSupersample);
    end;

    try
      Interpreter := TSVGInterpreter.Create(XML, ChromaPNG.Canvas, OpacityPNG.Canvas, InitialViewbox, InitialTransformation, FinalSupersample, AlphaDisabled, 1);
      Interpreter.Free();

      // Zusammenlegen der Bilder
      //OpacityPNG.SaveToFile('opacity.png');
      //ChromaPNG.SaveToFile('chroma.png');
      if (FinalSupersample > 1) and Downsample then
      if AlphaDisabled then
      begin
        InitBlankNonPaletteImage(COLOR_RGB, 8, Round(Dimensions.x), Round(Dimensions.y));
        TempPNG := Self;
        PNGResize3to1RGB(ChromaPNG, True, TempPNG, False); // Self übergeben geht nicht
        OpacityPNG.Free();
      end
      else
      begin
        InitBlankNonPaletteImage(COLOR_RGBALPHA, 8, Round(Dimensions.x), Round(Dimensions.y));
        JoinAndDownscale(ChromaPNG, OpacityPNG, Self, True);
      end
      else
      OpacityPNG.Free();

      Exit; // nur erstes <svg> in der Wurzel bearbeiten
    except
      OpacityPNG.Free();
      if ChromaPNG <> Self then
      ChromaPNG.Free();
      raise;
    end;
  end;
end;

procedure TSVGImage.LoadFromString(const s: string);
var
  XML: TRedeemerXML;
begin
  XML := TRedeemerXML.Create(s);
  try
    LoadFromString(XML);
  finally
    XML.Free();
  end;
end;

procedure TSVGInterpreter.LoadBrush(const Fill: TFill);
begin
  ChromaCanvas.Brush.Color := Fill.Color; // wenn man das nach Style setzt, geht das nicht
  if (Fill.Color = clNone) or (Fill.Opacity < 0.2) then
  ChromaCanvas.Brush.Style := bsClear
  else
  ChromaCanvas.Brush.Style := bsSolid;
  if SameText(XML.CurrentTag, '/text') then // text-Tag wird an seinem Ende geparst
  SetPolyFillMode(ChromaCanvas.Handle, WINDING) // Grafikfehler bei Text mit Linien darüber oder dadurch verhindern
  else
  SetPolyFillMode(ChromaCanvas.Handle, Fill.Rule);
  OpacityCanvas.Brush.Color := clWhite;
  OpacityCanvas.Brush.Style := ChromaCanvas.Brush.style;
  if SameText(XML.CurrentTag, '/text') then // text-Tag wird an seinem Ende geparst
  SetPolyFillMode(OpacityCanvas.Handle, WINDING) // Grafikfehler bei Text mit Linien darüber oder dadurch verhindern
  else
  SetPolyFillMode(OpacityCanvas.Handle, Fill.Rule);

  if Fill.Color = clDefault then
  begin
    DeleteObject(ChromaCanvas.Brush.Handle);
    // sinnlose Typumwandlung wegen Compilterärger
    ChromaCanvas.Brush.Handle := CreateDIBPatternBrushPt(TMemoryStream(Patterns[Fill.Pattern]).Memory, DIB_RGB_COLORS)
  end;
end;

procedure TSVGInterpreter.LoadPen(const Stroke: TStroke; const Context: TSVGContext);
var
  Flags: Cardinal;
  Width: Integer;
  Brush: tagLOGBRUSH;
  Dashes: TCoordinates;
  DashData: packed array of DWord;
  f, PercentageScale, Scale: Extended;
  Miter: Single;
begin
  if (Stroke.Color = clNone) or (Stroke.Width = 0) or (Stroke.Opacity <= 1/18) then
  Flags := PS_NULL // Width kann nicht auf weniger als 1 gesetzt werden, weder mit ExtCreatePen noch mit TPen
  else
  Flags := PS_SOLID;

  PercentageScale := sqrt((sqr(Context.LastViewBox.Width) + sqr(Context.LastViewBox.Height)) / 2);
  if Stroke.VectorEffect then
  Scale := FinalSupersample * NonScalingStrokeFactor
  else
  Scale := sqrt((sqr(Context.Transformations.a)+sqr(Context.Transformations.b)+sqr(Context.Transformations.c)+sqr(Context.Transformations.d))/2);

  if Flags <> PS_NULL then
  begin
    Width := Round(TCoordinates.MakeAbsolute(PercentageScale, Stroke.Width)*Scale);
    if Stroke.DashArray <> 'none' then
    begin
      Dashes := TCoordinates.Create(Stroke.DashArray);
      try
        while Dashes.GetNextCoordinate(PercentageScale, f) do
        begin
          SetLength(DashData, Length(DashData) + 1);
          DashData[High(DashData)] := Round(f*Scale);
          Flags := PS_USERSTYLE;
          if Length(DashData) = 16 then
          Break; // undokumente Einschränkung von Length(DashData) auf <= 16 in GDI
        end;
        // Deaktivierung eines Sonderfalls in GDI, das ein ungerade lange Dasharrays bei ungerade Wiederholungen invertiert durchläuft
        if Length(DashData) mod 2 = 1 then
        begin
          SetLength(DashData, Length(DashData) + 1);
          DashData[High(DashData)] := 0;
        end;
      finally
        Dashes.Free;
      end;
    end;
  end
  else
  Width := 0;

  Miter := Stroke.Miterlimit * 0.9999999; // GDI schneidet AB der Gehrungsgrenze ab, SVG schneider ÜBER der Grenze ab
  if (Stroke.Opacity < 17/18) and ((Stroke.Color = clWhite) or (Stroke.Color = clBlack)) then
  begin
    Brush.lbStyle := BS_DIBPATTERNPT;
    Brush.lbColor := DIB_RGB_COLORS;
    CurrentPenChromaSloppyDIB1.Create(clWhite, Stroke.Opacity);
    //CurrentPenChromaSloppyDIB2.Create(Stroke.Color, Stroke.Opacity);
    Brush.lbHatch := Integer(@Self.CurrentPenChromaSloppyDIB1);
  end
  else
  begin
    Brush.lbStyle := BS_SOLID;
    Brush.lbColor := Stroke.Color;
  end;
  DeleteObject(ChromaCanvas.Pen.Handle);
  ChromaCanvas.Pen.Handle := ExtCreatePen(PS_GEOMETRIC or Flags or Stroke.Linecap or Stroke.Linejoin, Width, Brush, Length(DashData), DashData);
  SetMiterLimit(ChromaCanvas.Handle, Miter, nil);
  if (Stroke.Opacity < 17/18) {and ((Stroke.Color = clWhite) or (Stroke.Color = clBlack))} then
  begin
    if Stroke.Color = clBlack then
    SetROP2(ChromaCanvas.Handle, R2_MASKNOTPEN); // Pixel, die im DIB weiß sind, werden schwarz
    if Stroke.Color = clWhite then
    SetROP2(ChromaCanvas.Handle, R2_MERGEPEN); // Pixel, die im DIB weiß sind, werden weiß
    CurrentPenOpacitySloppyDIB.Create(clWhite, Stroke.Opacity);
    Brush.lbStyle := BS_SOLID;
    Brush.lbColor := clWhite;
    Brush.lbHatch := Integer(@Self.CurrentPenOpacitySloppyDIB);
  end
  else
  begin
    Brush.lbStyle := BS_SOLID;
    Brush.lbColor := clWhite;
  end;
  DeleteObject(OpacityCanvas.Pen.Handle);
  OpacityCanvas.Pen.Handle := ExtCreatePen(PS_GEOMETRIC or Flags or Stroke.Linecap or Stroke.Linejoin, Width, Brush, Length(DashData), DashData);
  //SetROP2(OpacityPNG.Canvas.Handle, R2_MERGEPEN); // stört beim OpacityPNG niemals
  SetMiterLimit(OpacityCanvas.Handle, Miter, nil);
end;

class procedure TSVGInterpreter.MakeViewportTransformation(var Target: TAffineTransformation; const ViewBox: TRealRect; const Dimensions: TRealPoint; const Align: TRealPoint; const Meet: Boolean);
var
  Scale: Extended;
begin
  if Align.x = -1 then
  Target := AffineTransformation(
            AffineScale(Dimensions.x / ViewBox.Width, Dimensions.y / ViewBox.Height),
            Target)
  else
  begin
  if Meet then
  Scale := Min(Dimensions.x / ViewBox.Width, Dimensions.y / ViewBox.Height)
  else
  Scale := Max(Dimensions.x / ViewBox.Width, Dimensions.y / ViewBox.Height);
  Target := AffineTransformation(
            AffineTransformation(Scale, 0, 0, Scale,
            (Dimensions.x - ViewBox.Width * Scale) * Align.x,
            (Dimensions.y - ViewBox.Height * Scale) * Align.y),
            Target);
  end;
  Target := AffineTransformation(AffineTranslation(-ViewBox.Left, -ViewBox.Top), Target);
end;

class procedure TSVGImage.PopUndoCallback;
begin
  if Length(CallbackStack) > 0 then
  begin
    SizeCallback := CallbackStack[High(CallbackStack)];
    SetLength(CallbackStack, Length(CallbackStack) - 1);
  end;
end;

class procedure TSVGImage.PushSetCallback(Event: TSizeCallbackEvent);
begin
  SetLength(CallbackStack, Length(CallbackStack) + 1);
  CallbackStack[High(CallbackStack)] := SizeCallback;
  SizeCallback := Event;
end;

class procedure TSVGInterpreter.ReadAspectRatio(XML: TRedeemerXML; out Align: TRealPoint; out Meet: Boolean);
var
  Splitter: TStyleSplitter;
begin
  Align := RealPoint(0.5, 0.5); // Align gibt die Position im Rechteck in Anteilen an (oder x=-1 bei none)
  Meet := True;
  Splitter := TStyleSplitter.Create(XML.GetAttribute('preserveAspectRatio'), True);
  try
    if Length(Splitter.Values) >= 1 then
    begin
      if Splitter.Values[0] = 'none' then Align := RealPoint(-1,0) else
      if Splitter.Values[0] = 'xMinYMin' then Align := RealPoint(0, 0) else
      if Splitter.Values[0] = 'xMidYMin' then Align := RealPoint(0.5, 0) else
      if Splitter.Values[0] = 'xMaxYMin' then Align := RealPoint(1, 0) else
      if Splitter.Values[0] = 'xMinYMid' then Align := RealPoint(0, 0.5) else
      if Splitter.Values[0] = 'xMaxYMid' then Align := RealPoint(1, 0.5) else
      if Splitter.Values[0] = 'xMinYMax' then Align := RealPoint(0, 1) else
      if Splitter.Values[0] = 'xMidYMax' then Align := RealPoint(0.5, 1) else
      if Splitter.Values[0] = 'xMaxYMax' then Align := RealPoint(0, 1);
      if Length(Splitter.Values) >= 2 then
      Meet := Splitter.Values[1] <> 'slice';
    end;
  finally
    Splitter.Free;
  end;
end;

class procedure TSVGInterpreter.ReadDimensions(XML: TRedeemerXML; var Dimensions: TRealPoint; const LastViewBox: TRealRect);
function GetOnlyValueDef(const Attribute: string; const PercentageMeasure: Extended; const Default: Extended): Extended;
var
  s: string;
  b: Boolean;
begin
  b := XML.GetAttribute(Attribute, s);
  if b then
  Result := TCoordinates.GetOnlyValueDef(s, PercentageMeasure, Default)
  else
  Result := Default;
end;
begin
  Dimensions.x := GetOnlyValueDef('width', LastViewBox.Width, LastViewBox.Width);
  Dimensions.y := GetOnlyValueDef('height', LastViewBox.Height, LastViewBox.Height);
end;

procedure TSVGInterpreter.ReadFill(var Fill: TFill);
var
  s, s1: string;
  f: Extended;
  TempColor: TColor;
begin
  // Füllung
  if GetProperty('fill',True,True,s) then
  if s <> 'inherit' then
  begin
    if (s = 'none') or (s = 'transparent') then
    Fill.Color := clNone
    else
    if GetColorExt(s, TempColor) then
    Fill.Color := TempColor
    else if StartsText('url(#', s) then // Musterreferenz
    begin
      if not TStyleSplitter.GetBracket(s, s1, Fill.Pattern) then Exit;
      if not SameText(s1, 'url') then Exit;
      Delete(Fill.Pattern, 1, 1);
      if not Patterns.ContainsKey(Fill.Pattern) then Exit;
      Fill.Color := clDefault; // reserviert für Muster
    end;
  end;

  // Deckfähigkeit (wird entweder als transparent oder nicht interpretiert)
  if GetProperty('fill-opacity', True, True, s) then
  if TCoordinates.GetOnlyValue(s, f) then
  Fill.Opacity := f;

  // Umgang mit Überschneidungen
  if GetProperty('fill-rule',True,True,s) then
  if s = 'nonzero' then
  Fill.Rule := WINDING
  else
  if s = 'evenodd' then
  Fill.Rule := ALTERNATE;
end;

procedure TSVGInterpreter.ReadFont(var Font: TCSSFont);
var
  s: string;
  temp: Extended;
  fonts: TStyleSplitter;
  success: Boolean;
procedure SetFont(const Name: string);
begin
  Font.Family := Name;
  success := True;
end;
begin
  // Schriftfarbe
  if GetProperty('font-family', True, True, s) then
  begin
    success := False;
    fonts := TStyleSplitter.Create(s, False);
    for s in fonts.Values do
    begin
      if SameText(s, 'sans-serif') then // case-insensitive
      SetFont('Arial')
      else
      if SameText(s, 'serif') then
      SetFont('Times New Roman')
      else
      if SameText(s, 'fantasy') then
      SetFont('Comic Sans MS')
      else
      if SameText(s, 'cursive') then
      SetFont('Mistral')
      else
      if SameText(s, 'monospace') then
      SetFont('Courier New')
      else
      begin
        if Screen.Fonts.IndexOf(s) > -1 then // standardmäßig case-insensitive, hier auch richtig
        SetFont(s);
      end;
      if Success then Break;
    end;
    fonts.Free;
  end;

  // Schriftgröße, % bezieht sich auf die vorherige Einstellung
  if GetProperty('font-size', True, True, s) then
  if TCoordinates.GetOnlyValue(s, temp)  then
  if temp < 0 then
  Font.Size := Font.Size*temp
  else
  Font.Size := temp;

  // Kursiv
  if GetProperty('font-style', True, True, s) then
  if (s = 'oblique') or (s = 'italic') then
  Font.Style := True
  else
  if (s = 'normal') then
  Font.Style := False;

  // Fett
  if GetProperty('font-weight', True, True, s) then
  if (s = 'bolder') or (s = 'bold') or (s = '600') or (s = '700') or (s = '800') or (s = '900') then
  Font.Weight := True
  else
  if (s = 'normal') or (s = 'lighter') or (s = 'light') or (s = '100') or (s = '200') or (s = '300') or (s = '400') or (s = '500') then
  Font.Weight := False;

  // Dekoration
  if GetProperty('text-decoration', True, True, s) then
  begin
    Font.LineThrough := False; // Reset wenn vorhanden
    Font.Underline := False; // Reset wenn vorhanden
    fonts := TStyleSplitter.Create(s, True);
    for s in fonts.Values do
    begin
      if SameText(s, 'underline') then // case-insensitive
      Font.Underline := True
      else
      if SameText(s, 'line-through') then
      Font.LineThrough := True;
    end;
    fonts.Free;
  end;
end;

function TSVGInterpreter.ReadPosition: TRealPoint;
begin
  Result.x := GetOnlyValueDef('x', 0);
  Result.y := GetOnlyValueDef('y', 0);
end;

procedure TSVGInterpreter.ReadStroke(var Stroke: TStroke);
var
  s: string;
  f: Extended;
  TempColor: TColor;
begin
  // Farbe
  if GetProperty('stroke',True,True,s) then
  if (s = 'none') or (s = 'transparent') then
  Stroke.Color := clNone
  else
  if GetColorExt(s, TempColor) then
  Stroke.Color := TempColor;

  // Deckfähigkeit (wird entweder als transparent oder nicht interpretiert)
  if GetProperty('stroke-opacity', True, True, s) then
  if TCoordinates.GetOnlyValue(s, f) then
  Stroke.Opacity := f;

  // Gehrungsgrenze
  if GetProperty('stroke-miterlimit', True, True, s) then
  if TCoordinates.GetOnlyValue(s, f) then
  Stroke.Miterlimit := f;

  // Breite
  if GetProperty('stroke-width',True,True,s) then
  if s = 'none' then // stroke-width: none; hat keinen Einfluss auf stroke-width
  Stroke.Color := clNone
  else
  Stroke.Width := TCoordinates.GetOnlyValueDef(s, -1, Stroke.Width);

  // Enden
  if GetProperty('stroke-linecap',True,True,s) then
  if SameText(s, 'butt') then
  Stroke.Linecap := PS_ENDCAP_FLAT
  else
  if SameText(s, 'round')  then
  Stroke.Linecap := PS_ENDCAP_ROUND
  else
  if SameText(s, 'square') then
  Stroke.Linecap := PS_ENDCAP_SQUARE;

  // Ecken
  if GetProperty('stroke-linejoin',True,True,s) then
  if SameText(s, 'miter') then
  Stroke.Linejoin := PS_JOIN_MITER
  else
  if SameText(s, 'round')  then
  Stroke.Linejoin := PS_JOIN_ROUND
  else
  if SameText(s, 'bevel') then
  Stroke.Linejoin := PS_JOIN_BEVEL;

  // Strichelungen (können prozentual sein und werden deshalb später erst interpretiert)
  if GetProperty('stroke-dasharray',True,True,s) then
  Stroke.Dasharray := s;

  // nur explizite Vererbung möglich, implizit geht nicht!
  s := XML.GetAttributeDef('vector-effect','');
  if SameText(s, 'non-scaling-stroke') then
  Stroke.VectorEffect := True
  else
  if not SameText(s, 'inherit') then
  Stroke.VectorEffect := False;
end;

function TSVGInterpreter.ReadStyle(var Context: TSVGContext): Boolean;
var
  s, Name, Content: string;
  steps: TStyleSplitter;
  params: TCoordinates;
  x1, x2, x3, x4, x5, x6: Extended;
  i: Integer;
begin
  Result := False;

  //if XML.CurrentTag = 'g' then
  {if XML.GetAttribute('id', s) then
  if not Symbols.ContainsKey(s) then
  Symbols.Add(s, XML.Position);}

  // Sichtbarkeit laden, ggf. abbrechen, da display auch das Zeichnen aller Kinder verhindert
  if not Context.Display then Exit;
  CurrentStyle := TStyle.Create(XML.GetAttributeDef('style', ''));
  try
    if GetProperty('display', True, True, s) then
    Context.Display := Context.Display and (s <> 'none');
    if not Context.Display then Exit;

    // Deckfähigkeit (wird entweder als transparent oder nicht interpretiert)
    if GetProperty('opacity', True, True, s) then
    if TCoordinates.GetOnlyValue(s, x1) then
    Context.Opacity := Context.Opacity * x1; // berechnet sich anders als Füllungs- und Konturendeckkraft relativ
    Context.Display := not (Context.Opacity < 0.2);
    if not Context.Display then Exit;

    // Zeichenreihenfolge laden
    if GetProperty('paint-order', True, True, s) then
    Context.PaintOrderStroke := s = 'stroke';

    if XML.GetAttribute('text-anchor', s) then
    if s = 'end' then
    Context.TextAnchor := TA_RIGHT
    else
    if s = 'middle' then
    Context.TextAnchor := TA_CENTER
    else
    if s = 'inherit' then
    else
    Context.TextAnchor := TA_LEFT;


    // Füllungs-, Konturen- und Schrifteigenschaften laden
    ReadFill(Context.Fill);
    ReadStroke(Context.Stroke);
    ReadFont(Context.Font);

    // Affine Abbildungen laden
    if XML.GetAttribute('transform', s) then // keine CSS-Eigenschaft!
    try
      steps := TStyleSplitter.Create(s, True);
      try
        for i := Low(Steps.Values) to High(Steps.Values) do // es werden immer neue innere (d.h. als erstes (direkt nach Rückgängigmachung von TempScale) auszuführende!) Transformationen angehängt
        begin
          TStyleSplitter.GetBracket(Steps.Values[i], Name, Content);
          params := TCoordinates.Create(Content);
          try
            // Translation, Name ist übrigens case-sensitive
            if Name = 'translate' then
            begin
              if params.GetNextCoordinate(x1) and params.GetNextCoordinate(x2) then
              Context.Transformations := AffineTransformation(AffineTranslation(x1, x2), Context.Transformations);
            end else
            // Drehung
            if Name = 'rotate' then
            begin
              if params.GetNextCoordinate(x1) then
              if params.GetNextCoordinate(x2) and params.GetNextCoordinate(x3) then
              Context.Transformations := AffineTransformation(AffineRotation(x1, x2, x3), Context.Transformations)
              else
              Context.Transformations := AffineTransformation(AffineRotation(x1), Context.Transformations);
            end else
            // Streckung und Stauchung
            if Name = 'scale' then
            begin
              if params.GetNextCoordinate(x1) then
              if params.GetNextCoordinate(x2) then
              Context.Transformations := AffineTransformation(AffineScale(x1, x2), Context.Transformations)
              else
              Context.Transformations := AffineTransformation(AffineScale(x1, x1), Context.Transformations);
            end else
            // 2 verschiedene Scherungen
            if Name = 'skewX' then
            begin
              if params.GetNextCoordinate(x1) then
              Context.Transformations := AffineTransformation(AffineSkewX(x1), Context.Transformations);
            end else
            if Name = 'skewY' then
            begin
              if params.GetNextCoordinate(x1) then
              Context.Transformations := AffineTransformation(AffineSkewY(x1), Context.Transformations);
            end else
            // Affine Abbildung
            if Name = 'matrix' then
            begin
              if params.GetNextCoordinate(x1) and params.GetNextCoordinate(x2) and params.GetNextCoordinate(x3) and
                 params.GetNextCoordinate(x4) and params.GetNextCoordinate(x5) and params.GetNextCoordinate(x6) then
              Context.Transformations := AffineTransformation(AffineTransformation(x1,x2,x3,x4,x5,x6), Context.Transformations);
            end;
          finally
            params.Free;
          end;
        end;
      finally
        steps.Free;
      end;
    except
    end;

    Result := True;
  finally
    CurrentStyle.Free;
  end;
end;

class function TSVGInterpreter.ReadViewbox(XML: TRedeemerXML; var ViewBox: TRealRect): Boolean;
var
  Coords: TCoordinates;
  Value: string;
  TempViewbox: TRealRect;
begin
  Result := XML.GetAttribute('viewbox', Value);
  if Result then
  begin
    Coords := TCoordinates.Create(Value);
    try
      Result := Coords.GetNextCoordinate(TempViewbox.Left) and
                Coords.GetNextCoordinate(TempViewbox.Top) and
                Coords.GetNextCoordinate(TempViewbox.Width) and
                Coords.GetNextCoordinate(TempViewbox.Height);
      if Result then
      ViewBox := TempViewbox; // nur setzen, wenn vollständig geladen
    finally
      Coords.Free;
    end;
  end;
end;

{ TCustomUTF8Encoding }

constructor TCustomUTF8Encoding.Create;
begin
  inherited Create(CP_UTF8, 0, 0); // TEncoding.UTF8 ohne MB_ERR_INVALID_CHARS
end;

{ TSloppyTransparencyDIB }

constructor TSloppyTransparencyDIB.Create(Color: TColor; Transparency: Extended);
var
  r, g, b, i: byte;
begin
  Bitmapinfo.biSize := 40;
  Bitmapinfo.biWidth := 3;
  Bitmapinfo.biHeight := 3;
  Bitmapinfo.biPlanes := 1;
  Bitmapinfo.biBitCount := 24;
  Bitmapinfo.biCompression := BI_RGB;
  Bitmapinfo.biSizeImage := 36; // 12 pro Zeile, da die 9 Bytes auf 12 aufgefüllt werden müssen
  Bitmapinfo.biXPelsPerMeter := 0;
  Bitmapinfo.biYPelsPerMeter := 0;
  Bitmapinfo.biClrUsed := 0;
  Bitmapinfo.biClrImportant := 0;
  b := Color and $FF;
  g := Color shr 8 and $FF;
  r := Color shr 16 and $FF;
  for i := 0 to Round(Transparency * 9) - 1 do
  begin
    Data[(i*4 div 3)*3 ] := b; // komische Berechnung wegen: siehe oben
    Data[(i*4 div 3)*3+1] := g;
    Data[(i*4 div 3)*3+2] := r;
  end
end;

{ TPatternDIBClass }

constructor TPatternDIBClass.Create(PNG: TPNGImage);
var
  LongBoundaryRowSize: Integer;
  i: Integer;
  Bitmapinfo: tagBITMAPINFOHEADER;
begin
  LongBoundaryRowSize := Math.Ceil(PNG.Width * 3 / 4) * 4; // Größe pro Zeile in Bytes
  SetSize(40 + LongBoundaryRowSize * PNG.Height);
  Bitmapinfo.biSize := 40;
  Bitmapinfo.biWidth := PNG.Width;
  Bitmapinfo.biHeight := PNG.Height;
  Bitmapinfo.biPlanes := 1;
  Bitmapinfo.biBitCount := 24;
  Bitmapinfo.biCompression := BI_RGB;
  Bitmapinfo.biSizeImage := LongBoundaryRowSize * PNG.Height;
  Bitmapinfo.biXPelsPerMeter := 0;
  Bitmapinfo.biYPelsPerMeter := 0;
  Bitmapinfo.biClrUsed := 0;
  Bitmapinfo.biClrImportant := 0;
  Write(Bitmapinfo, 40);
  for i := Bitmapinfo.biHeight - 1 downto 0 do
  Write((PByteArray(PNG.Scanline[i])^)[0], LongBoundaryRowSize);
end;

{ TSVGMetafile }

constructor TSVGMetafile.Create();
begin
  inherited;
  Callback := nil;
  NonScalingStrokeFactor := 1;
  Self.Enhanced := True;
  WordFix := False;
end;

procedure TSVGMetafile.FixRDPBug(const Handle: HDC);
var
  Temp: TMemoryStream;
  Header: PENHMETAHEADER;
begin
  // Workaround für einen Fehler in RDP, durch den szlMillimeters und rclFrame falsch sind, wenn der Hauptbildschirm des verbindenden Rechners nicht das Seitenverhältnis 4:3 hat
  // vgl. https://stackoverflow.com/a/1533053
  // Kann leider nicht beim geladenen Bild geändert werden, da man an szlDevice (HORZRES und VERTRES bei GetDeviceCaps) nicht im Speicher des vorhandenen Bildes ran kommt
  // Bug scheint in Windows 10 nicht mehr zu existieren.
  // Nachtrag 15.6.21: Dafür existiert ein ähnlicher, auf dieselbe Weise behebbarer Bug:
  //                   - Beim ursprünglichen RDP-Bug waren HORZSIZE und VERTSIZE zwar gleich szlMillimeters, aber falsch in Relation und im Verhältnis zu HORZRES und VERTRES.
  //                   - Beim neuen Bug sind HORZSIZE und VERTSIZE gleich szlMillimeters und korrekt, aber HORZRES und VERTRES sind ungleich szlDevice.

  // Besteht KEINE Remote-Desktop-Verbindung? Dann Abbruch.
  // Keine Ahnung, ob das Anfang August funktioniert hat, aber ich habe jetzt Ende Oktober keinen Self.Handle mehr, die Methode gibt daher 0 zurück, was anders als 320 bzw. 240 ist.
  // Daher wird der Handle von TMetafileCanvas übergeben.
  //if GetDeviceCaps(Handle, HORZSIZE) <> 320 then Exit;
  //if GetDeviceCaps(Handle, VERTSIZE) <> 240 then Exit;
  // Theoretisch können diese Werte auch ohne RDP auftreten, wenn der Nutzer einen 4:3-Bildschirm besitzt.
  // FixRDPBug macht korrekte Bilder nicht kaputt, daher geht es hier ausschließlich um die Performance, die bei einem Durchlauf des folgenden Codes verloren geht.
  // 15.6.21: Obige Zeilen auskommentiert da Kriterien für Bildschirmskalierungs-Bug unzureichend. Kriterien können erst nach dem Laden bestimmt werden.

  Temp := TMemoryStream.Create();
  try
    Self.SaveToStream(Temp);
    //Self.SaveToFile('debug1.emf');
    if Temp.Size > 0 then
    begin
      Temp.Position := 0;
      Header := Temp.Memory;

      if (Header^.szlDevice.cx = GetDeviceCaps(Handle, HORZRES)) and
         (Header^.szlDevice.cy = GetDeviceCaps(Handle, VERTRES)) then
      Exit;

      Header^.rclFrame.Right := (Header^.rclBounds.Right + 1) * 100;
      Header^.rclFrame.Bottom := (Header^.rclBounds.Bottom + 1) * 100;
      Header^.szlDevice.cx := 320; // gleiche Werte für beide szl-Felder machen es einfacher, Rundungsfehler hierüber zu vermeiden (man kann einfach mit 100 multiplizieren)
      Header^.szlDevice.cy := 240;
      Header^.szlMillimeters.cx := 320;
      Header^.szlMillimeters.cy := 240;
      //Header := nil;

      //Temp.SaveToFile('debug2.emf');

      inherited LoadFromStream(Temp);
    end;
  finally
    Temp.Free();
  end;
end;

procedure TSVGMetafile.LoadFromStream(Stream: TStream);
var
  Encoding: TCustomUTF8Encoding;
  sl: TStringList;
begin
  sl := TStringList.Create;
  Encoding := TCustomUTF8Encoding.Create;
  try
    sl.LoadFromStream(Stream, Encoding);
    LoadFromString(sl.Text);
  finally
    sl.Free;
    Encoding.Free;
  end;
end;

procedure TSVGMetafile.LoadFromString(const s: string);
var
  XML: TRedeemerXML;
begin
  XML := TRedeemerXML.Create(s);
  try
    LoadFromString(XML);
  finally
    XML.Free();
  end;
end;

procedure TSVGMetafile.LoadFromString(XML: TRedeemerXML);
var
  Meet: Boolean;
  Align, Dimensions: TRealPoint;
  InitialViewbox: TRealRect;
  InitialTransformation: TAffineTransformation;
  Interpreter: TSVGInterpreter;
  DummyPNG: TPNGImage;
  MetaCanvas: TMetafileCanvas;
begin
  while XML.GoToAndGetNextTag do
  if (XML.CurrentTag = 'svg') then
  begin
    // Erstmal Größe lesen
    InitialViewbox := RealRect(0, 0, 300, 300);
    //ReadPosition;
    TSVGInterpreter.ReadAspectRatio(XML, Align, Meet);
    if TSVGInterpreter.ReadViewbox(XML, InitialViewbox) then
    TSVGInterpreter.ReadDimensions(XML, Dimensions, InitialViewbox)
    else
    begin
      TSVGInterpreter.ReadDimensions(XML, Dimensions, InitialViewbox);
      InitialViewbox.Width := Dimensions.x;
      InitialViewbox.Height := Dimensions.y;
    end;

    // Größe vom Benutzer bestätigen lassen
    if Assigned(Callback) then
    Callback(Self, InitialViewbox, Dimensions);

    // Standard-Bosstransformation: Koordinatensystem umwandeln
    InitialTransformation := AffineTransformation(1, 0, 0, 1, -0.5 + Byte(WordFix), -0.5 + Byte(WordFix)); // warum das -0,5 sein muss, kann man sich bei Koordinaten vorstellen, die auf ,5 enden.

    // Transformation in den richtigen Zeichenbereich
    TSVGInterpreter.MakeViewportTransformation(InitialTransformation, InitialViewbox, Dimensions, Align, Meet); // keine Position im Wurzel-Tag

    //Self.Width := 4711; // UniqueImage auslösen
    SetSize(Round(Dimensions.x) + Byte(WordFix), Round(Dimensions.y) + Byte(WordFix));

    DummyPNG := TPNGImage.CreateBlank(COLOR_RGB, 8, 1, 1);


    try
      MetaCanvas := TMetafileCanvas.Create(Self, DummyPNG.Canvas.Handle);
      // Width und Height sind hiernach 0
      try
        Interpreter := TSVGInterpreter.Create(XML, MetaCanvas, DummyPNG.Canvas, InitialViewbox, InitialTransformation, 1, True, NonScalingStrokeFactor); // Alpha ist zwar nicht disabled, aber wir brauchen uns darum nicht kümmern
        Interpreter.Free();
      finally
        MetaCanvas.Free();
      end;

      // Hier gab es ein Problem:
      // Aus irgendeinem Grund ist neuerdings(?) Self leer (SaveToFile ist ergibt 0 Bytes und Handle ist 0), bis MetaCanvas freigegeben wird.
      // Anschließend ist zwar Handle <> 0, GetScreenCaps gibt aber 0 zurück.
      // MetaCanvas ist aber schon freigegeben, hat also auch keinen Handle mehr. Daher nehmen wir das Handle von DummyPNG.
      if Enhanced then // Wenn SPÄTER (Enhanced bezieht sich auf den Zeitpunkt des Speicherns, was aber auch Methodenaufruf verursachen würde) als WMF gespeichert wird, enthalten keine Auflösungsdaten
      FixRDPBug(DummyPNG.Canvas.Handle);

      Exit; // nur erstes <svg> in der Wurzel bearbeiten
    finally
      DummyPNG.Free();
    end;
  end;
end;

initialization
  TPicture.RegisterFileFormat('SVG', 'Scalable Vector Graphics', TSVGImage);
finalization
  TPicture.UnregisterGraphicClass(TSVGImage);

end.
