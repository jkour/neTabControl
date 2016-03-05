unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  Unit5, neTabControl, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm4 = class(TForm)
    GridPanelLayout1: TGridPanelLayout;
    BasicFrame1: TBasicFrame;
    Button1: TButton;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    customeTabControl: TneTabControl;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
var
  tmpItem: TneTabItem;
  tmpFrame: TFrame;
  x: Integer;
begin
  tmpItem:=TneTabItem.Create(customeTabControl);
  tmpItem.Text:='hfkajhfkasdjhfl '+random(100).ToString;

  x:=random(20);
  if x=0 then x:=3;

  if (1000 mod x)=0 then
     tmpItem.CanClose:=false;

  if (1456 mod x)=0 then
     tmpItem.ShowIcon:=false;

  //customeTabControl.DisablePopupMenu:=true;

  tmpFrame:=TFrame.Create(tmpItem);

  customeTabControl.AddTab('Tab'+random.ToString,tmpItem,tmpFrame);

end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  customeTabControl:=TneTabControl.Create(self);
  customeTabControl.Parent:=BasicFrame1;
  customeTabControl.Align:=TAlignLayout.Client;

end;

end.
