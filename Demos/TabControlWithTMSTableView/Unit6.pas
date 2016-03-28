unit Unit6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  neTabControl, FMX.TMSBaseControl, FMX.TMSTableView, FMX.Layouts;

type
  TForm6 = class(TForm)
    TMSFMXTableView1: TTMSFMXTableView;
    StyleBook1: TStyleBook;
    neTabControl1: TneTabControl;
    GridPanelLayout1: TGridPanelLayout;
    GridPanelLayout2: TGridPanelLayout;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.FormCreate(Sender: TObject);
var
  tmpItem: TneTabItem;
  tmpFrame: TFrame;
begin
  tmpItem:=TneTabItem.Create(neTabControl1);
  tmpItem.Text:='sdfasdf';
  tmpFrame:=TFrame.Create(tmpItem);
  neTabControl1.AddTab('TabA', tmpItem, tmpFrame);

  tmpItem:=TneTabItem.Create(neTabControl1);
  tmpItem.Text:='sdfase345345df';
  tmpFrame:=TFrame.Create(tmpItem);
  neTabControl1.AddTab('TabB', tmpItem, tmpFrame);

end;

end.
