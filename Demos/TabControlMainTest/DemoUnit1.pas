unit DemoUnit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, neTabControl,
  FMX.Controls.Presentation, FMX.TabControl, FMX.Edit, FMX.Menus, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.Objects;

type
  TForm2 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    StyleBook1: TStyleBook;
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    Button4: TButton;
    CheckBox1: TCheckBox;
    Button5: TButton;
    Edit2: TEdit;
    Button6: TButton;
    Button7: TButton;
    ListBox2: TListBox;
    CheckBox2: TCheckBox;
    Edit3: TEdit;
    Button8: TButton;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    CheckBox3: TCheckBox;
    Memo1: TMemo;
    Button13: TButton;
    Image1: TImage;
    Label2: TLabel;
    Button14: TButton;
    CheckBox4: TCheckBox;
    RadioButton3: TRadioButton;
    Label3: TLabel;
    Button15: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure RadioButton3Change(Sender: TObject);
    procedure Button15Click(Sender: TObject);
  private
    procedure UpdateTagList;
    function TestOnBeforeDelete(const AItem: TneTabItem): boolean;
    procedure TestOnAfterDelete(ADeletedItem: TneTabItem; ADeletedFrame: TFrame);
    procedure OnMenuLastItemClick (Asender: TObject);
    procedure UpdateListBox2;
    procedure OnTabChange(Sender: TObject);
  public
    customNETabContol: TNETabControl;
  end;

  TMyTab=class(TTabItem)

  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses Unit1, FMX.Styles.Objects,
  Unit3;


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

procedure TForm2.Button10Click(Sender: TObject);
begin
  if customNETabContol.TabCount>0 then
    customNETabContol.Next;
end;


procedure TForm2.Button11Click(Sender: TObject);
begin
  if customNETabContol.TabCount>0 then
    customNETabContol.Previous;
end;

procedure TForm2.Button12Click(Sender: TObject);
var
  tmpItem: TTabItem;
  tmpFrame: TFrame3;
begin
  if Trim(Edit1.Text)='' then
    ShowMessage('Empty tag')
  else
  begin
    tmpItem:=TTabItem.Create(customNETabContol);
    tmpItem.Text:=Trim(Edit1.Text);

    tmpFrame:=TFrame3.Create(tmpItem);
    tmpFrame.Label1.Text:=Random(10000).ToString;

    customNETabContol.AddTab(Trim(Edit1.Text), tmpItem, TFrame(tmpFrame));
    UpdateTagList;
  end;
end;

procedure TForm2.Button13Click(Sender: TObject);
var
  tmpSavedList: TStringList;
  i: Integer;
  str: string;
begin
  Memo1.Lines.Clear;
  if CheckBox4.IsChecked then
    str:=customNETabContol.ActiveTag
  else
    str:='';
  tmpSavedList:=TStringList.create;
  customNETabContol.SaveTabs([ExportTagInt, ExportIdentifier, ExportTagVariant], [str],tmpSavedList);
  for i := 0 to tmpSavedList.Count-1 do
    Memo1.Lines.Add(tmpSavedList[i]);
  tmpSavedList.Free;
end;


procedure TForm2.Button14Click(Sender: TObject);
begin
  Label2.Text:=customNETabContol.ActiveTag;
end;

procedure TForm2.Button15Click(Sender: TObject);
begin
  customNETabContol.DeleteAllTabs(CheckBox3.IsChecked);
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  tmpFrame: TFrame3;
begin
  tmpFrame:=TFrame3.Create(nil);
  tmpFrame.Label1.Text:=Random(10000).ToString;
  customNETabContol.AddTab('Tab '+random(1000).ToString, TFrame(tmpFrame));
  UpdateTagList;
end;

procedure TForm2.UpdateTagList;
var
  i: integer;
  tmpListItem: TListBoxItem;
  tmpList,
  tmpList1: TStringList;
begin
  ListBox1.Clear;
  tmpList:=customNETabContol.GetTabsTags;
  tmpList1:=customNETabContol.GetTabsText;
  for i:=0 to tmpList.Count-1 do
  begin
    tmpListItem:=TListBoxItem.Create(ListBox1);
    tmpListItem.Parent:=ListBox1;
    tmpListItem.Text:=tmpList1.Strings[i]+': '+tmpList.Strings[i]+
        ' ('+customNETabContol.GetTab(tmpList.Strings[i]).Index.ToString+')';
    tmpListItem.TagString:=tmpList.Strings[i];
    ListBox1.AddObject(tmpListItem);
  end;

  if ListBox1.Count>0 then
    ListBox1.ItemIndex:=0;

  if Assigned(tmpList) then
    tmpList.Free;
  if Assigned(tmpList1) then
    tmpList1.Free;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  tmpItem: TneTabItem;
  tmpFrame: TFrame3;
begin
  if Trim(Edit1.Text)='' then
    ShowMessage('Empty tag')
  else
  begin
    tmpItem:=TneTabItem.Create(customNETabContol);
    tmpItem.Text:=Trim(Edit1.Text);
    tmpItem.Tag:=Random(100);
    tmpItem.Identifier:='ABC';
    tmpItem.TagVariant:='John '+random(10).ToString;

    if customNETabContol.TabCount mod 2=0 then
    begin
      tmpItem.CanClose:=true;
    end
    else
      tmpItem.CanClose:=false;

    tmpFrame:=TFrame3.Create(tmpItem);
    tmpFrame.Label1.Text:=Random(10000).ToString;

    customNETabContol.AddTab(Trim(Edit1.Text), tmpItem, TFrame(tmpFrame));
    UpdateTagList;
  end;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  if (ListBox1.Count=0) or (ListBox1.ItemIndex=-1) then Exit;
  customNETabContol.SetActiveTab(ListBox1.Selected.TagString);
end;


procedure TForm2.Button4Click(Sender: TObject);
begin
  if (ListBox1.Count=0) or (ListBox1.ItemIndex=-1) then Exit;
  if checkBox3.IsChecked then
    customNETabContol.DeleteTab(ListBox1.Selected.TagString, true)
  else
    customNETabContol.DeleteTab(ListBox1.Selected.TagString);
  UpdateTagList;
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  tmpFrame: TFrame1;
begin
  tmpFrame:=TFrame1.Create(nil);
  tmpFrame.Label1.Text:=Random(10000).ToString;
  customNETabContol.InsertTab('Tab '+random(1000).ToString,
                          Random(customNETabContol.TabCount), TFrame(tmpFrame));
  UpdateTagList;
end;

procedure TForm2.Button6Click(Sender: TObject);
var
  tmpItem: TneTabItem;
  tmpFrame: TFrame1;
begin
  if Trim(edit2.Text)='' then Edit2.Text:='0';
  if Trim(Edit1.Text)='' then
  begin
    ShowMessage('Empty tag');
    Exit;
  end;

  if Edit2.Text.ToInteger>customNETabContol.TabCount-1 then
    ShowMessage('The index must be smaller than the tab count')
  else
  begin
    tmpItem:=TneTabItem.Create(customNETabContol);
    tmpItem.Text:=Trim(Edit1.Text);

    if customNETabContol.TabCount mod 2=0 then
    begin
      tmpItem.CanClose:=true;
    end
    else
      tmpItem.CanClose:=false;

    tmpFrame:=TFrame1.Create(tmpItem);
    tmpFrame.Label1.Text:=Random(10000).ToString;

    customNETabContol.InsertTab(Trim(Edit1.Text),
                        edit2.Text.ToInteger, tmpitem, TFrame(tmpFrame));
    UpdateTagList;
  end;
end;


procedure TForm2.Button7Click(Sender: TObject);
var
  tmp: TMyTab;
  s: string;
begin
  tmp:=TabControl1.Insert(1, TMyTab) as TMyTab;
  s:=TabControl1.TabCount.ToString;
  tmp.Text:='inserted '+s;
  UpdateListBox2;
end;

procedure TForm2.Button8Click(Sender: TObject);
begin
  if TabControl1.TabCount>1 then
  begin
    TabControl1.Tabs[TabControl1.TabCount-1].Index:=0;
  end;
end;

procedure TForm2.Button9Click(Sender: TObject);
var
  tmpImage: TBitmap;
begin
  tmpImage:=customNETabContol.GetFrame(Edit1.Text).MakeScreenshot;
  Image1.Bitmap:=tmpImage;
  tmpImage.Free;
end;

procedure TForm2.CheckBox1Change(Sender: TObject);
begin
  if (ListBox1.Count=0) or (ListBox1.ItemIndex=-1) then Exit;
  customNETabContol.GetTab(ListBox1.Selected.TagString).ShowIcon:=
    CheckBox1.IsChecked;
end;

procedure TForm2.CheckBox2Change(Sender: TObject);
begin
  TabControl1.Tabs[Edit3.Text.ToInteger].Visible:=CheckBox2.IsChecked;
  UpdateListBox2;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  tmpPop1, tmpPop2: TPopupMenu;
  tmpPopItem: TMenuItem;
  tmpImage: TBitmap;
begin
  customNETabContol:=TNETabControl.Create(self);
  customNETabContol.Parent:=self;
  customNETabContol.Align:=TAlignLayout.MostTop;
  customNETabContol.Margins.Left:=20;
  customNETabContol.Margins.Right:=20;
  customNETabContol.Margins.Top:=20;
  customNETabContol.Height:=300;
  customNETabContol.OnBeforeDelete:=TestOnBeforeDelete;
  customNETabContol.OnAfterDelete:=TestOnAfterDelete;
  customNETabContol.OnChange:=OnTabChange;
  customNETabContol.CloseTabOnDoubleClick:=true;

  tmpPop1:=TPopupMenu.Create(customNETabContol);
  tmpPopItem:=TMenuItem.Create(tmpPop1);
  tmpPopItem.Text:='Added on';
  tmpPop1.AddObject(tmpPopItem);

  tmpPopItem:=TMenuItem.Create(customNETabContol);
  tmpPopItem.Text:='-';
  tmpPop1.AddObject(tmpPopItem);

  customNETabContol.PopupBeforeDefault:=tmpPop1;

  tmpPop2:=TPopupMenu.Create(self);
  tmpPopItem:=TMenuItem.Create(tmpPop2);
  tmpPopItem.Text:='-';
  tmpPop2.AddObject(tmpPopItem);

  tmpPopItem:=TMenuItem.Create(self);
  tmpPopItem.text:='After dark';
  tmpPopItem.OnClick:=OnMenuLastItemClick;
  tmpPop2.AddObject(tmpPopItem);

  customNETabContol.PopupAfterDefault:=tmpPop2;

  GetImageFromResources('CloseNormal', tmpImage);
  customNETabContol.CloseImageNormal:=tmpImage;
  GetImageFromResources('CloseHover', tmpImage);
  customNETabContol.CloseImageHover:=tmpImage;
  GetImageFromResources('ClosePressed', tmpImage);
  customNETabContol.CloseImagePressed:=tmpImage;

  customNETabContol.HintStyle:=THintStyle.ShowTitle;

  customNETabContol.StyleLookup:='neTabControlStyle';

  RadioButton1.IsChecked:=true;

end;


procedure TForm2.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  Label1.Text:='X: '+X.ToString+' - Y: '+Y.ToString;
end;

procedure TForm2.OnMenuLastItemClick(Asender: TObject);
var
  tmp: TMenuItem;
begin
  if Asender is TMenuItem then
  begin
    tmp:=ASender as TMenuItem;
    ShowMessage(tmp.text+':'+tmp.TagString);
  end;
end;

procedure TForm2.OnTabChange(Sender: TObject);
begin
  inherited;
  Label3.Text:=customNETabContol.TabIndex.ToString;
end;

procedure TForm2.RadioButton1Change(Sender: TObject);
begin
  if RadioButton1.IsChecked then
    customNETabContol.HintStyle:=THintStyle.ShowTitle;
end;

procedure TForm2.RadioButton2Change(Sender: TObject);
begin
  if RadioButton2.IsChecked then
    customNETabContol.HintStyle:=THintStyle.ShowPreview;
end;

procedure TForm2.RadioButton3Change(Sender: TObject);
begin
  if RadioButton3.IsChecked then
    customNETabContol.HintStyle:=THintStyle.None;
end;

procedure TForm2.TestOnAfterDelete(ADeletedItem: TneTabItem; ADeletedFrame: TFrame);
begin
  if Assigned(ADeletedItem) then
  begin
    ShowMessage('Item deleted: '+ADeletedItem.Text);
    ADeletedItem.Free;

    if Assigned(ADeletedFrame) then
      ShowMessage('Label: '+TFrame3(ADeletedFrame).Label1.Text);
  end;
  UpdateTagList;
end;

procedure TForm2.UpdateListBox2;
var
  i: Integer;
  tmpItem: TListBoxItem;
begin
  ListBox2.Clear;
  for i := 0 to TabControl1.TabCount - 1 do
  begin
    tmpitem := TListBoxItem.Create(ListBox2);
    tmpitem.Text := TabControl1.Tabs[i].Text+': '+tabcontrol1.Tabs[i].Index.ToString;
    ListBox2.AddObject(tmpitem);
  end;
end;

function TForm2.TestOnBeforeDelete(const AItem: TneTabItem): boolean;
begin
  inherited;

  ShowMessage('On before delete');
  result:=true;
end;



end.
