unit Kollegah;

interface

uses
  RedeemerAffineGeometry, Windows, Graphics, Sysutils;

type
  TPoints = packed array of tagPOINT;

procedure Bosstransformation(const SourcehDC: Cardinal; const TargethDC: array of Cardinal; const Transformation: TAffineTransformation; const StrokeFirst: Boolean);

implementation

procedure Bosstransformation(const SourcehDC: Cardinal; const TargethDC: array of Cardinal; const Transformation: TAffineTransformation; const StrokeFirst: Boolean);
var
  Points: TPoints;
  Types: TBytes;
  Count: Integer;
  i: Cardinal;
  j: Integer;
begin
  Count := GetPath(SourcehDC, {$IF CompilerVersion >= 30.0}@{$IFEND}Points[0], {$IF CompilerVersion >= 30.0}@{$IFEND}Types[0], 0);
  SetLength(Points, Count);
  SetLength(Types, Count);
  GetPath(SourcehDC, {$IF CompilerVersion >= 30.0}@{$IFEND}Points[0], {$IF CompilerVersion >= 30.0}@{$IFEND}Types[0], Count);

  for j := Low(Types) to Count - 1 do
  Points[j] := AffineTransformation(Transformation, Points[j]);
  for i in TargethDC do
  begin
    AbortPath(i);
    if StrokeFirst then
    PolyDraw(i, Points[0], Types[0], Count);
    BeginPath(i);
    PolyDraw(i, Points[0], Types[0], Count);
    EndPath(i);
    FillPath(i);
    // Zeichne die Linie einzeln (statt mit StrokeAndFillPath), da ohne aktiven Pfad auch Sprünge funktionieren
    if not StrokeFirst then
    PolyDraw(i, Points[0], Types[0], Count);
  end;
end;

end.
