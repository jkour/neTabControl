unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, neTabControl;

type
  TFrame4 = class(TFrame)
    neTabControl1: TneTabControl;
  private
    { Private declarations }
  public
    procedure Setup;
  end;

implementation

{$R *.fmx}


{ TFrame4 }

procedure TFrame4.Setup;
var
  tmpItem: TneTabItem;
  tmpFrame: Tframe;
begin
  tmpItem:=TneTabItem.Create(neTabControl1);
  tmpItem.Text:='Academic';
  tmpItem.ShowIcon:=false;
  tmpItem.CanClose:=true;

  tmpFrame:=TFrame.Create(nil);

  neTabControl1.AddTab('New',tmpItem, tmpFrame);
end;

end.
