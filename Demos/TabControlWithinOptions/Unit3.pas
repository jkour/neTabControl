unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.TabControl, neTabControl;

type
  TForm3 = class(TForm)
    Button1: TButton;
    neTabControl1: TneTabControl;
    StyleBook1: TStyleBook;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses Unit4;

procedure TForm3.Button1Click(Sender: TObject);
var
  tmpOptions: TOptionsTemplateView4;
begin
  tmpOptions:=TOptionsTemplateView4.Create(self);
  tmpOptions.Show;

end;

procedure TForm3.FormCreate(Sender: TObject);
var
  tmpItem: TneTabItem;
  tmpFrame: TFrame;
begin
  tmpItem:=TneTabItem.Create(neTabControl1);
  tmpItem.Text:='14534fdfdgdfgs53454';

  tmpFrame:=TFrame.Create(nil);

  neTabControl1.AddTab('Main New',tmpItem,tmpFrame);
end;

end.
