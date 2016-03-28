unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  neTabControl, FMX.Controls.Presentation, FMX.StdCtrls, FMX.WebBrowser,
  FMX.Menus;

type
  TForm4 = class(TForm)
    neTabControl1: TneTabControl;
    ButtonNewPage: TButton;
    StyleBook1: TStyleBook;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    ButtonOptions: TButton;
    Button2: TButton;
    ButtonLoad: TButton;
    Button1: TButton;
    procedure ButtonNewPageClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    fSavedTabs: TStringList;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

uses Unit2, Unit5, System.StrUtils;

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

procedure TForm4.ButtonOptionsClick(Sender: TObject);
var
  optionsTab: TneTabItem;
  optionsFrame: TFrameOptions;
begin
  optionsTab:=TneTabItem.Create(neTabControl1);
  optionsTab.Text:='Options';
  optionsTab.Identifier:='Options';

  optionsFrame:=TFrameOptions.Create(nil);

  neTabControl1.AddTab('Options',optionsTab, TFrame(optionsFrame));
  neTabControl1.SetActiveTab('Options');
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  neTabControl1.DeleteAllTabs(true);
end;

procedure TForm4.Button2Click(Sender: TObject);
begin
  fSavedTabs.Free;
  fSavedTabs:=TStringList.Create;
  neTabControl1.SaveTabs([ExportIdentifier, ExportTagVariant], [], fSavedTabs);
  if fSavedTabs.Count>0 then
    ButtonLoad.Enabled:=true
  else
    ButtonLoad.Enabled:=false;
end;

procedure TForm4.ButtonLoadClick(Sender: TObject);
var
  tmpStr: array[0..2] of string;
  tmpDyn: TStringDynArray;
  str: string;
  newTab: TneTabItem;
  newFrame: TFrameBrowser;
begin
  if Assigned(fSavedTabs) and (fSavedTabs.Count>0) then
  begin
    for str in fSavedTabs do
    begin
     tmpDyn:=splitString(str,',');   //tag - identifier - TagVariant

     if tmpDyn[1]='Options' then
       ButtonOptionsClick(sender);
     if tmpDyn[1]='Web Page' then
     begin
        newTab:=TneTabItem.Create(neTabControl1);
        newTab.CanClose:=true;
        newTab.ShowIcon:=true;

        newFrame:=TFrameBrowser.Create(nil);
        newTab.Text:=tmpDyn[2];
        newTab.Identifier:='Web Page';
        newTab.TagVariant:=newTab.Text;

        neTabControl1.AddTab(tmpdyn[0],newTab, TFrame(newFrame));
     end;
    end;
  end;
end;

procedure TForm4.ButtonNewPageClick(Sender: TObject);
var
  newTab: TneTabItem;
  newFrame: TFrameBrowser;
  newF: TFrame;
begin
  newTab:=TneTabItem.Create(neTabControl1);
  newTab.CanClose:=true;
  newTab.ShowIcon:=true;

  newFrame:=TFrameBrowser.Create(nil);
  newTab.Text:=newFrame.Edit1.Text;
  newTab.Identifier:='Web Page';
  newTab.TagVariant:=newTab.Text;

  neTabControl1.AddTab('Page: '+neTabControl1.TabCount.ToString,newTab, TFrame(newFrame));

end;


procedure TForm4.FormCreate(Sender: TObject);
var tmpImage: TBitMap;
begin
  GetImageFromResources('CloseNormal', tmpImage);
  if Assigned(tmpImage) then
    neTabControl1.CloseImageNormal:=tmpImage;

  GetImageFromResources('ClosePressed', tmpImage);
  if Assigned(tmpImage) then
    neTabControl1.CloseImagePressed:=tmpImage;

  GetImageFromResources('CloseHover', tmpImage);
  if Assigned(tmpImage) then
    neTabControl1.CloseImageHover:=tmpImage;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  fSavedTabs.Free;
end;

procedure TForm4.MenuItem1Click(Sender: TObject);
var
  tmpTabItem: TneTabItem;
begin
  if not (Sender is TMenuItem) then exit;
  tmpTabItem:=neTabControl1.GetTab((Sender as TMenuItem).TagString);
  if not Assigned(tmpTabItem) then Exit;
  Showmessage('View Source of: '+tmpTabItem.Text);
end;

procedure TForm4.RadioButton1Click(Sender: TObject);
begin
  if RadioButton1.IsPressed then
    neTabControl1.HintStyle:=THintStyle.ShowTitle;
end;

procedure TForm4.RadioButton2Click(Sender: TObject);
begin
  if RadioButton2.IsPressed then
    neTabControl1.HintStyle:=THintStyle.ShowPreview;
end;

end.
