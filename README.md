# RedeemerSVG

Vielseitiger SVG-zu-GDI-Interpreter für Delphi

Entstanden 2017 mit einigen Patches und Ergänzungen bis 2021


## Features

- Übersetzt SVG nach GDI
  - Falls eine Eigenschaft von `TCanvas` alle damit verbundenen GDI-Features kapselt, wird stattdessen diese genutzt. Zum Zeitpunkt der Entwicklung war insbesondere `TPen` zu unvollständig um benutzbar zu sein.
- `TSVGInterpreter` unterstützt die Ausgabe auf beliebigen Gerätekontexten
- Mitgelieferte Ausgabemöglichkeiten:
  - als `TPNGImage`-Erbe (`TSVGImage`)
  - als `TMetafile`-Erbe (`TSVGMetafile`)
  - auf `TPrinter` über Unterstützerklasse
- Ausgabe ohne unnötige Verluste – eine Vektorgrafik zu drucken, ergibt beispielsweise eine Vektorgrafik
- Ist eines der extrem wenigen Produkte, die SVGZ-_Dateien_ laden können
- Unterstützt seit jeher SVG-Features, die bis heute in Browsern oder kommerziellen Produkten fehlen, beispielsweise `solidColor` und die allerabsurdsten Pfad-Schreibweisen wie Arc-Flags ohne Trennzeichen
- FSAA-MSAA durch 3×-Supersampling mit ungewichtetem Downsampling (weil gewichtetes Downsampling hässlich ist, insbesondere bei Nicht-Fotos)
- Callback-Events zur Änderung der Darstellungsgröße sind auf folgende Arten nutzbar:
  - `TSVGImage`: Eigenschaft des Objekts, globale Variable `SizeCallback` oder Stack-Methoden von `TSVGImage`
  - `TSVGMetafile`: Eigenschaft des Objekts
  - `TPrinter`: Nicht unterstützt (druckt proportional vollflächig in vom Gerätekontext vorgebener Auflösung)


## Voraussetzungen

- Delphi 2009 oder neuer (wegen Generics, UTF-16 und Unterstützerklassen), getestet mit 12.0

Irgendwann wurde von Emba die Methodensignatur von `GetPath` geändert. Es erwartet nun Zeiger als zweites und drittes Argument. Die genaue Version ist mir nicht bekannt, aber laut einem [Thread auf Stack Overflow](https://stackoverflow.com/questions/32333148/delphi-10-seattle-changes-to-win32-getpath-and-redundant-tpoint-and-pointl-reco) DCC 30.0. Entsprechend ist die bedingte Kompilierung ausgeführt.


## Verwendungsbeispiele

### TSVGImage

```pascal
SVG := TSVGImage.Create();
try
  SVG.LoadFromFile(FileName);
  Image1.Picture.Assign(SVG);
finally
  SVG.Free;
end;
```


### TSVGMetafile

Durch die Ausgabe als Metafile können SVGs auch dann als Vektorgrafik ausgegeben werden, wenn andere Komponenten das nicht können, aber EMF oder WMF unterstützten. `TSVGMetafile` unterstützt zudem optional einen Workaround (genannt `WordFix`) für einen Bug in Microsoft Office, das Metafiles fälschlicherweise mit dem Vektorgrafik-Koordinatensystem lädt. Metafiles erfordern aber _zwingend_ ein Rastergrafik-Koordinatensystem (unter anderem, weil ihr Koordinatensystem ganzzahlig ist). Würde eine Metafile ohne den Workaround in Word geladen, würde es oben und links jeweils abgeschnitten. Der Bug existiert auch in einigen anderen Programmen. Die Eigenschaft Enhanced kann ebenfalls genutzt werden. Beispielsweise konnten alte Versionen von List&Label EMFs nicht mit dem richtigen Seitenverhältnis laden, WMFs aber schon. Bézierkurven werden zwar von GDI automatisch in Segmente unterteilt, es fehlen jedoch einige Stift-Features ersatzlos.

```pascal
SVG := TSVGMetafile.Create();
try
  //SVG.Enhanced := ...;
  //SVG.WordFix := ...;
  SVG.LoadFromFile(FileName);
  if SVG.Enhanced then
  SVG.SaveToFile(ChangeFileExt(FileName, '.emf'))
  else
  SVG.SaveToFile(ChangeFileExt(FileName, '.wmf'));
finally
  SVG.Free();
end;
```

Es ist eine Überlegung wert, im Callback ein großzügiges Supersampling anzufordern, zumal es hier keinen Speicher kostet.


### TPrinter

Man kann beliebig viele SVGs auf eine Seite malen, aber sie nehmen jeweils die gesamte Seite ein (proportional). Man kann auch mehrere Seiten mit verschiedenen SVGs ausgeben.

Es kann derzeit keine Dateien laden sondern nur Strings und Streams.

```
if PrintDialog.Execute() then
begin
  SL := TStringList.Create();
  try
    SL.LoadFromFile(FileName);
    //Printer.Orientation := poLandscape;
    Printer.Title := ChangeFileExt(ExtractFileName(Filename), '');
    Printer.BeginDoc();
    Printer.PrintSVGFromString(SL.Text);
    //Printer.NewPage();
    //Printer.PrintSVGFromString(...);
    Printer.EndDoc();
  finally
    SL.Free();
  end;
end;
```


## SVG-Umfang

`TSVGInterpreter` unterstützt diejenigen Features aus SVGTiny 2.0, die einigermaßen direkt in GDI existieren.

SVGTiny ist ein wilder Mischmasch aus anderen SVG-Versionen und teils darüber hinaus, aber ohne Stylesheet-Unterstützung. `style`-Attribute funktionen. Illustrator-Exporte funktionieren nicht. Affinity (zumindest Affinity 1) funktioniert. Weil ich Affinity benutze, ist RedeemerSVG auf maximale Kompatibilität mit diesem ausgelegt.

GDI unterstützt beispielsweise keine Verläufe (von denen diese Klasse nur die erste definitierte Farbe rendert), richtige Transparenz, Filter und exotische Text-Eigenschaften.

Bei Pfaden fehlen Catmull-Rom und Bearing, allerdings hatten neben mir auch alle Browser-Hersteller keine Lust auf die zwei, weshalb das W3C sie aus SVG 2.0 wieder entfernt hat. Gleiches gilt für die nicht von mir unterstützten Werte von `vector-effect`, die ebenfalls zur Entfernung vorgesehen sind.

| Eigenschaft | `style` | Wert | SVG | Unterstützt | Anmerkungen |
| ----------- | ------- | ---- | ------- | ------------- | ----------- |
| `fill` | ja | Hex-Farbcode |  | ohne Alpha | Standard: schwarz |
|  |  | `rgb()` |  | voll |  |
|  |  | `rgba()` |  | ohne Alpha |  |
|  |  | `hsl()` |  | voll |  |
|  |  | `hsla()` |  | ohne Alpha |  |
|  |  | `cymk()` | ab 2.0? | keine |  |
|  |  | Namen |  | voll |  |
|  |  | `rebeccapurple` |  | voll |  |
|  |  | `none` |  | voll |  |
|  |  | `currentColor` |  | keine |  |
|  |  | `url(#`...`)` |  | kein Verlauf |  |
| `fill-opacity` | ja | $[0;1]$ |  | experimentell |  |
| `fill-rule` | ja | `evenodd` |  | voll |  |
|  |  | `nonzero` |  | voll |  |
| `stroke` | ja | Hex-Farbcode |  | ohne Alpha |  |
|  |  | `rgb()` |  | voll |  |
|  |  | `rgba()` |  | ohne Alpha |  |
|  |  | `hsl()` |  | voll |  |
|  |  | `hsla()` |  | ohne Alpha |  |
|  |  | `cymk()` |  | keine |  |
|  |  | Namen |  | voll |  |
|  |  | `rebeccapurple` |  | voll |  |
|  |  | `none` |  | voll |  |
|  |  | `currentColor` |  | keine |  |
|  |  | `url(#`...`)` |  | kein Verlauf |  |
| `stroke-opacity` | ja | $[0;1]$ |  | experimentell |  |
| `stroke-alignment` | ja |  | ab 2.0 | keine | teilweise mit GDI `WidenPath` möglich? |
| `stroke-linecaps` | ja | `bevel` |  | voll |  |
|  |  | `round` |  | voll |  |
|  |  | `square` |  | voll |  |
| `stroke-linejoin` | ja | `miter` |  | voll | >`miter-limit` wie `bevel` (ab 2.0) |
|  |  | `miter-clip` | ab 2.0 | keine | >`miter-limit` abflachen, beeinflusst Strichelung |
|  |  | `round` |  | voll |  |
|  |  | `bevel` |  | voll |  |
|  |  | `arcs` | ab 2.0 | keine |  |
| `stroke-miter-limit` | ja | dimensionslos |  | voll | nur bei `stroke-linejoin:miter` |
| `stroke-dasharray` | ja | siehe `stroke-width` |  | &#x7c;·&#x7c; $≤16$ | Einschränkung durch GDI |
| `stroke-width` | ja | keine Einheit |  | voll | wie Pixel |
|  |  | Pixel |  | voll |  |
|  |  | Prozentangabe |  | voll |  |
|  |  | `none` |  | voll |  |
| `vector-effect` | nein | none | ab 2.0 | voll |  |
|  |  | `non-scaling-stroke` |  | voll | nur explizit vererbbar (mit `inherited`!) |
|  |  | `non-scaling-size` |  | keine | entfällt aus SVG |
|  |  | `non-rotation` |  | keine | entfällt aus SVG |
|  |  | `fixed-position` |  | keine | entfällt aus SVG |
| `font-family` | ja | eine Schriftart |  | voll | eine Eigenschaft `font` gibt es nicht! |
|  |  | `serif` |  | voll |  |
|  |  | `sans-serif` |  | voll |  |
|  |  | `cursive` |  | voll |  |
|  |  | `fantasy` |  | voll |  |
|  |  | `monospace` |  | voll |  |
|  |  | Font-Stacks |  | voll |  |
| `font-size` | ja | keine Einheit |  | voll | wie Pixel |
|  |  | Pixel |  | voll |  |
|  |  | Punkt |  | voll |  |
|  |  | Prozentangabe |  | voll |  |
|  |  | Terme |  | keine |  |
| `font-style` | ja | `normal` |  | voll |  |
|  |  | `italic` |  | voll |  |
|  |  | `oblique` |  | wie `italic` | macht Firefox auch so |
| `font-weight` | ja | `normal` |  | voll |  |
|  |  | `bold` |  | voll |  |
|  |  | `bolder` |  | wie `bold` | keine Berechnung aus vorherigem Wert |
|  |  | $100n \mid n \in ℕ \cap [1;9]$ |  | voll | ≥600 wie `bold` (wie in Firefox) |
| `transformation` | nein | `rotate` |  | voll |  |
|  |  | `scale` |  | voll |  |
|  |  | `skewX` |  | voll |  |
|  |  | `skewY` |  | voll |  |
|  |  | `translate` |  | voll |  |
|  |  | `transform` |  | voll |  |
|  |  | Konkatenation |  | voll |  |
| `paint-order` | ja | `stroke` | ab 2.0 | voll | Notation überhaupt nicht intuitiv |
|  |  | _(anderes)_ |  | voll |  |
| `opacity` | ja | $[0;1]$ |  | <0,2 = trans |  |
| `display` | ja | `none` |  | voll |  |
|  |  | _(anderes)_ |  | voll |  |


## Hinweise

- Infos zur grundlegenden Funktionsweise finden sich im [Thread auf Delphi-PRAXiS](https://www.delphipraxis.net/193635-redeemersvg-tsvgimage-kleine-svg-unit-fuer-delphi-mit-gdi.html) (die Dokumentation hier auf Github ist im Zweifel aktueller als die dortige)
- Es gibt einen GDI+-basierten Interpreter für Delphi. Dieser unterstützt naturgemäß wesentlich mehr SVG-Features als RedeemerSVG, allerdings fehlt ihm die Vielseitigkeit durch die verlustfreie Ausgabe auf unterschiedlichsten Gerätekontexten.
- Die gzip-Unterstützung ist etwas friemelig, weil Z-Streams in Delphi 2009 und 2010 (und XE?) keine negativen Fenster unterstützen und sich daher normalerweise nur für zlib-Dateien nutzen lassen. Um diese völlig grund- und sinnlose Einschränkung zu umgehen und beliebige Deflate-Streams zu unterstützen, muss man den – völlig sinnlosen – zlib-Header hinzufügen oder entfernen.
- Unterstützt RDP. Auf Metafile-Gerätekontexte kann normalerweise *nur* fehlerfrei gezeichnet werden, wenn die Remotedesktop-Bildschirmauflösung 320x240 beträgt. Bei anderen 4:3-Auflösungen heben sich die Auswirkungen Bug immerhin gegenseitig wieder auf. Wird jedoch beispielsweise ein 16:9-Bildschirm für RDP verwendet, sind durch Metafile-Gerätekontexte gezeichnete horizontale Linien sehr viel dicker als vertikale, da GDI an einigen Stellen mit der tatsächlichen Auflösung, aber an anderen Stellen mit 320x240 rechnet. Der aus Windows 7 bekannte Bug existiert unter Windows 10 nicht mehr, dafür existiert ein neuer, ähnlicher Bug. RedeemerSVG umgeht beide Bugs, wenn nötig. Der Bugfix ist so gebaut, dass seine grundlose Anwendung nicht zu Problemen führt, sollte RDP jemals andere Auflösungen als 320x240 vollständig unterstützen.
- Von Salat schrumpft der Bizeps.
