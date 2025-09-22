unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Menus, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    ImageControl1: TImageControl;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Skia, FMX.Skia;

function ResizeImageMitchell(const AFilename: String;
  const ANewWidth, ANewHeight: Integer): TBitmap;
var
  LImage: ISkImage;
  LSurface: ISkSurface;
  LCanvas: ISkCanvas;
  LPaint: ISkPaint;
  SrcRect, DstRect: TRectF;
  MitchellSampling: TSkSamplingOptions;
begin
  Result := nil;

  // Load your original image
  LImage := TSkImage.MakeFromEncodedFile(AFilename);

  if not Assigned(LImage) or (ANewWidth <= 0) or (ANewHeight <= 0) then
    Exit;

  // Create a new surface with the target dimensions
  LSurface := TSkSurface.MakeRaster(ANewWidth, ANewHeight);
  if not Assigned(LSurface) then
    Exit;

  LCanvas := LSurface.Canvas;

  // Create paint object
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  // Mitchell-Netravali cubic resampler (B=1/3, C=1/3)
  MitchellSampling := TSkSamplingOptions.Create(
    TSkCubicResampler.Create(1/3, 1/3)
  );

  // Define source and destination rectangles
  SrcRect := TRectF.Create(0, 0, LImage.Width, LImage.Height);
  DstRect := TRectF.Create(0, 0, ANewWidth, ANewHeight);

  // Clear the canvas
  LCanvas.Clear(TAlphaColors.Null);

  // Draw the image with Mitchell cubic resampling
  LCanvas.DrawImageRect(LImage, SrcRect, DstRect, MitchellSampling, LPaint);

  // Get the result image from the surface as a TBitmap
  Result := SKImageToBitmap(LSurface.MakeImageSnapshot);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  bmp: TBitmap;
begin
OpenDialog1.Filter := 'PNG Images (*.png)|*.png';
if(OpenDialog1.Execute) then
  begin
    bmp := ResizeImageMitchell(OpenDialog1.Filename, 800, 800);
    if(Assigned(bmp)) then
      begin
        ImageControl1.Bitmap := bmp;
        bmp.free;
      end;
  end;
end;

end.
