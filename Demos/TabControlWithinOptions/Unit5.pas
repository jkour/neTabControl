unit Unit5;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, neTabControl, FMX.Controls.Presentation;

type
  TFrame5 = class(TFrame)
    neTabControl1: TneTabControl;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    procedure FrameMouseEnter(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Setup;
  end;

implementation

{$R *.fmx}

{ TFrame5 }

procedure TFrame5.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.IsChecked then
     neTabControl1.Tabs[0].Width:=400
  else
  begin
    neTabControl1.Tabs[0].Width:=100;
  end;

  Label2.Text:=neTabControl1.Tabs[0].TagString+' | ' + neTabControl1.Tabs[0].Text;
end;

procedure TFrame5.FrameMouseEnter(Sender: TObject);
begin
  Label1.Text:='neTabControl: X: '+neTabControl1.Position.X.ToString+' | Y: '+
        neTabControl1.Position.Y.ToString+chr(13)+chr(10)+
        'Frame: X: '+self.Left.ToString+' | Y: '+self.Position.Y.ToString;
end;

procedure TFrame5.Setup;
var
  tmpItem: TneTabItem;
  tmpFrame: TFrame;
begin
  tmpItem:=TneTabItem.Create(neTabControl1);
  tmpItem.Text:='2342';

  tmpFrame:=TFrame.Create(nil);

  neTabControl1.AddTab('New',tmpItem,tmpFrame);

  tmpItem:=TneTabItem.Create(neTabControl1);
  tmpItem.Text:='22222222';

  tmpFrame:=TFrame.Create(nil);

  neTabControl1.AddTab('New Another',tmpItem,tmpFrame);
end;

end.
