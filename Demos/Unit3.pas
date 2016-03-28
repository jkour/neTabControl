unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  neTabControl, FMX.Objects;

type
  TForm3 = class(TForm)
    StyleBook1: TStyleBook;
    Rectangle1: TRectangle;
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

uses Unit4, Unit5;

procedure TForm3.FormCreate(Sender: TObject);
var
  tmpFrame: TFrame5;
  tmpFrame2: TFrame4;
begin
  tmpFrame:=TFrame5.Create(Rectangle1);
  tmpFrame.Parent:=self;
  tmpFrame.Align:=TAlignLayout.Client;

  tmpFrame2:=TFrame4.Create(nil);
  tmpFrame2.Parent:=tmpFrame;
  tmpFrame2.Align:=TAlignLayout.Contents;
  tmpFrame2.Setup;
end;

end.
