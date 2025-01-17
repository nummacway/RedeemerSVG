unit RedeemerSVGZ;

interface

uses
  RedeemerSVG, RedeemerGzip, Classes, zlib, Graphics;

type
  TSVGZImage = class(TSVGImage)
    public
      procedure LoadFromStream(Stream: TStream); override;
  end;

type
  TSVGZMetafile = class(TSVGMetafile)
    public
      procedure LoadFromStream(Stream: TStream); override;
  end;

implementation

{ TSVGZImage }

procedure TSVGZImage.LoadFromStream(Stream: TStream);
var
  Gzip: TRedeemerGzipAdapter;
  Unzip: TDecompressionStream;
begin
  Gzip := TRedeemerGzipAdapter.CreateFromCompressed(Stream);
  try
    Unzip := TDecompressionStream.Create(Gzip.Zlib);
    try
      inherited LoadFromStream(Unzip);
    finally
      Unzip.Free();
    end;
  finally
    Gzip.Free();
  end;
end;

{ TSVGZMetafile }

procedure TSVGZMetafile.LoadFromStream(Stream: TStream);
var
  Gzip: TRedeemerGzipAdapter;
  Unzip: TDecompressionStream;
begin
  Gzip := TRedeemerGzipAdapter.CreateFromCompressed(Stream);
  try
    Unzip := TDecompressionStream.Create(Gzip.Zlib);
    try
      inherited LoadFromStream(Unzip);
    finally
      Unzip.Free();
    end;
  finally
    Gzip.Free();
  end;
end;

initialization
  TPicture.RegisterFileFormat('SVGZ', 'Scalable Vector Graphics + Gzip', TSVGZImage);
finalization
  TPicture.UnregisterGraphicClass(TSVGZImage);

end.
