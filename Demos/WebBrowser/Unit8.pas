unit Unit8;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, neTabControl,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.TabControl, FMX.Edit;

type
  TForm8 = class(TForm)
    ButtonDownload1: TButton;
    neTabControl1: TneTabControl;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button2: TButton;
    Button3: TButton;
    StyleBook1: TStyleBook;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit1ChangeTracking(Sender: TObject);
    procedure ButtonDownload1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    AddButton: TSpeedButton;
    procedure OnAddButtonClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

uses
	FMX.Objects, Unit2, neTabItem, Unit3, FMX.Styles, Unit5;

{$R *.fmx}


procedure GetImageFromResources(const ImageName: string; var Image: TBitMap);
var
  InStream: TResourceStream;
begin
  if trim(ImageName)<>'' then
  begin
    InStream := TResourceStream.Create(HInstance, ImageName, RT_RCDATA);
    try
        Image:=TBitmap.Create;
        Image.Canvas.Bitmap.LoadFromStream(InStream);
    finally
      InStream.Free;
    end;
  end;
end;

procedure ResizeBitmap(B: TBitmap; width, height: Integer; var newBit: TBitmap);
begin
  newBit:=TBitmap.Create(width, height);
  newBit.Clear(0);
  if newBit.Canvas.BeginScene then
  try
    newBit.Canvas.DrawBitmap(B, RectF(0,0,b.Width, b.Height), RectF(0,0,width, height),1);
  finally
    newBit.Canvas.EndScene;
  end;
end;

procedure TForm8.ButtonDownload1Click(Sender: TObject);
var
  downTab: TneTabItem;
  downFrame: TFrame5;
  tmpRect: TCircle;
begin
  downTab:=TneTabItem.Create(self);
  downTab.Text:='Download 1';
  downTab.CanClose:=true;
  downTab.ShowControl:=True;
  downTab.MinTabWidth:=100;
  downTab.MaxTabWidth:=500;
  downTab.TabWidth:=400;

  tmpRect:=TCircle.Create(downTab);
  tmpRect.Fill.Color:=$FF000000 or TAlphaColor(random($FFFFFF));
  tmpRect.Width:=18;
  tmpRect.Height:=18;

  downTab.ControlToShow:=tmpRect;

  downFrame:=TFrame5.Create(self);
  downFrame.Align:=TAlignLayout.Contents;
  downFrame.neTabControl:=neTabControl1;
  downFrame.TagString:='Download 1';

  neTabControl1.AddTab('Download 1', downTab, TFrame(downFrame));

end;

procedure TForm8.Button2Click(Sender: TObject);
var
  optTab: TneTabItem;
  optFrame: TFrame3;
  tmpRect: TRectangle;
begin
  optTab:=TneTabItem.Create(self);
  optTab.Text:='Options';
  optTab.CanClose:=true;
  optTab.ShowControl:=True;

  tmpRect:=TRectangle.Create(optTab);
  tmpRect.Fill.Color:=$FF000000 or TAlphaColor(random($FFFFFF));
  tmpRect.Width:=18;
  tmpRect.Height:=18;

  optTab.ControlToShow:=tmpRect;

  optFrame:=TFrame3.Create(self);
  optFrame.Align:=TAlignLayout.Contents;
  optFrame.TagString:='Options';
  optFrame.ParentTabControl:=neTabControl1;

  neTabControl1.AddTab('Options', optTab, TFrame(optFrame));

end;

procedure TForm8.Button3Click(Sender: TObject);
begin
begin
  if OpenDialog1.Execute then
  begin
    TStyleManager.SetStyle(TStyleStreaming.LoadFromFile(OpenDialog1.FileName));
    Label2.Text:=ExtractFileName(OpenDialog1.FileName);
    neTabControl1.RefreshTabHeight(ExtractFileName(OpenDialog1.FileName));
  end;
end;
end;

procedure TForm8.Edit1ChangeTracking(Sender: TObject);
begin
  if Edit1.Text='' then
    neTabControl1.TabHeight:=27
  else
    neTabControl1.TabHeight:=edit1.Text.ToInteger;
end;

procedure TForm8.FormCreate(Sender: TObject);
var
  addBitmap,
  addResized: TBitmap;
  addImage: TImage;
  tmpImage: TBitmap;
begin
  GetImageFromResources('CloseNormal', tmpImage);
  neTabControl1.CloseImageNormal.Assign(tmpImage);
  GetImageFromResources('CloseHover', tmpImage);
  neTabControl1.CloseImageHover.Assign(tmpImage);
  GetImageFromResources('ClosePressed', tmpImage);
  neTabControl1.CloseImagePressed.Assign(tmpImage);

  GetImageFromResources('AddImage', addBitmap);
  AddButton:=TSpeedButton.Create(self);
  AddButton.Size.Width:=30;
  addImage:=TImage.Create(self);
  if Assigned(addBitmap) then
  begin
    ResizeBitmap(addBitmap, 16, 16, addResized);
    addImage.Width:=16;
    addImage.Height:=16;
    addImage.Align:=TAlignLayout.Center;
    addImage.HitTest:=false;
    addImage.Bitmap.Assign(addResized);
  end;

  AddButton.OnClick:=OnAddButtonClick;

  addImage.Parent:=AddButton;

  neTabControl1.AddSidebarControl(AddButton, TneSibebarControlPosition.sbLeft);
end;

procedure TForm8.FormResize(Sender: TObject);
begin
  if neTabControl1.TabOrientation=TneTabOrientation.orLeft then
  begin
    neTabControl1.TabOrientation:=TneTabOrientation.orTop;
    neTabControl1.TabOrientation:=TneTabOrientation.orLeft;
  end;
end;

procedure TForm8.OnAddButtonClick(Sender: TObject);
var
  newFrame: TFrame2;
  newTab: TnetabItem;
  tag: string;
  tmpImage: TImage;
  tmpBit,
  tmpBit2: TBitmap;
begin
  tag:='Page '+random(1000).ToString;
  newTab:=TneTabItem.Create(neTabControl1);
  newTab.Text:='http://www.google.com';
  newTab.CanClose:=true;

  tmpImage:=TImage.Create(newTab);
  tmpImage.Width:=16;
  tmpImage.Height:=16;

  GetImageFromResources('Star', tmpBit);
  if Assigned(tmpBit) then
  begin
    ResizeBitmap(tmpBit, 16, 16, tmpBit2);
    if Assigned(tmpBit2) then
    begin
      tmpImage.Bitmap.Assign(tmpBit2);
      newTab.ShowControl:=true;
      newTab.ControlToShow:=tmpImage;
    end;
  end;

  newTab.MaxTabWidth:=300;
  newTab.MinTabWidth:=200;
  newTab.TabWidth:=270;

  newFrame:=TFrame2.Create(neTabControl1);
  newFrame.ParentTabControl:=neTabControl1;
  newFrame.Edit1.Text:='http://www.google.com';
  newFrame.Align:=TAlignLayout.Contents;
  newFrame.Margins.Bottom:=30;
  newFrame.tagString:=tag;

  neTabControl1.AddTab(tag, newTab, TFrame(newFrame));
end;

procedure TForm8.RadioButton1Change(Sender: TObject);
begin
  neTabControl1.TabOrientation:=TneTabOrientation.orTop;
end;

procedure TForm8.RadioButton2Change(Sender: TObject);
begin
  neTabControl1.TabOrientation:=TneTabOrientation.orLeft;
end;

end.
