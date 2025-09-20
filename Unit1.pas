unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Menus, FMX.Skia, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ImageControl1: TImageControl;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure EncodeTestCubic(const AFilename: String);
    procedure EncodeTestMitchell(const AFilename: String);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

function ResizeSkiaImageWithCubicResampler(const AImage: ISkImage;
  const ANewWidth, ANewHeight: Integer): ISkImage;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  SamplingOptions: TSkSamplingOptions;
  Paint: ISkPaint;
  SrcRect, DstRect: TRectF;
begin
  Result := nil;

  if not Assigned(AImage) or (ANewWidth <= 0) or (ANewHeight <= 0) then
    Exit;

  // Create a new surface with the target dimensions
  Surface := TSkSurface.MakeRaster(ANewWidth, ANewHeight);
  if not Assigned(Surface) then
    Exit;

  Canvas := Surface.Canvas;

  // Create cubic resampler sampling options
  SamplingOptions := TSkSamplingOptions.Create(TSkCubicResampler.Mitchell);

  // Create paint object
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;

  // Define source and destination rectangles
  SrcRect := TRectF.Create(0, 0, AImage.Width, AImage.Height);
  DstRect := TRectF.Create(0, 0, ANewWidth, ANewHeight);

  // Clear the canvas
  Canvas.Clear(TAlphaColors.Null);

  // Draw the image with cubic resampling
  Canvas.DrawImageRect(AImage, SrcRect, DstRect, SamplingOptions, Paint);

  // Get the result image from the surface
  Result := Surface.MakeImageSnapshot;
end;

function ResizeSkiaImageMitchell(const AImage: ISkImage;
  const ANewWidth, ANewHeight: Integer): ISkImage;
var
  Surface: ISkSurface;
  Canvas: ISkCanvas;
  Paint: ISkPaint;
  SrcRect, DstRect: TRectF;
  MitchellSampling: TSkSamplingOptions;
begin
  Result := nil;

  if not Assigned(AImage) or (ANewWidth <= 0) or (ANewHeight <= 0) then
    Exit;

  // Create a new surface with the target dimensions
  Surface := TSkSurface.MakeRaster(ANewWidth, ANewHeight);
  if not Assigned(Surface) then
    Exit;

  Canvas := Surface.Canvas;

  // Create paint object
  Paint := TSkPaint.Create;
  Paint.AntiAlias := True;

  // Mitchell-Netravali cubic resampler (B=1/3, C=1/3)
  MitchellSampling := TSkSamplingOptions.Create(
    TSkCubicResampler.Create(1/3, 1/3)
  );

  // Define source and destination rectangles
  SrcRect := TRectF.Create(0, 0, AImage.Width, AImage.Height);
  DstRect := TRectF.Create(0, 0, ANewWidth, ANewHeight);

  // Clear the canvas
  Canvas.Clear(TAlphaColors.Null);

  // Draw the image with Mitchell cubic resampling
  Canvas.DrawImageRect(AImage, SrcRect, DstRect, MitchellSampling, Paint);

  // Get the result image from the surface
  Result := Surface.MakeImageSnapshot;
end;

procedure TForm1.EncodeTestCubic(const AFilename: String);
var
  OriginalImage, ResizedImage: ISkImage;
begin
  // Load your original image
  OriginalImage := TSkImage.MakeFromEncodedFile('c:\video\testframe.png');

  // Resize to 800x600 using cubic resampling
  ResizedImage := ResizeSkiaImageWithCubicResampler(OriginalImage, 800, 800);

  if Assigned(ResizedImage) then
  begin
    // Save or use the resized image
    ResizedImage.EncodeToFile('output.png');
  end;
end;

procedure TForm1.EncodeTestMitchell(const AFilename: String);
var
  OriginalImage, ResizedImage: ISkImage;
begin
  // Load your original image
  OriginalImage := TSkImage.MakeFromEncodedFile('c:\video\testframe.png');

  // Resize to 800x600 using cubic resampling
  ResizedImage := ResizeSkiaImageMitchell(OriginalImage, 800, 800);

  if Assigned(ResizedImage) then
  begin
    // Save or use the resized image
    ResizedImage.EncodeToFile('output-mitchell.png');
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
OpenDialog1.Filter := 'PNG Images (*.png)|*.png';
if(OpenDialog1.Execute) then
  begin
    EncodeTestCubic(OpenDialog1.Filename);
    if(FileExists('output.png')) then
      ImageControl1.LoadFromFile('output.png');
  end;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
OpenDialog1.Filter := 'PNG Images (*.png)|*.png';
if(OpenDialog1.Execute) then
  begin
    EncodeTestMitchell(OpenDialog1.Filename);
    if(FileExists('output-mitchell.png')) then
      ImageControl1.LoadFromFile('output-mitchell.png');
  end;

end;

end.
