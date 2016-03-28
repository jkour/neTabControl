unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.WebBrowser, FMX.Edit, FMX.Controls.Presentation, FMX.TMSWebBrowser;

type
  TFrameBrowser = class(TFrame)
    Label1: TLabel;
    Edit1: TEdit;
    ButtonLoadPage: TButton;
    Label2: TLabel;
    procedure ButtonLoadPageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  neTabControl;

procedure TFrameBrowser.ButtonLoadPageClick(Sender: TObject);
var
  tmpItem: TneTabItem;
  tmpObj: TFMXObject;
begin
  Label2.Text:=#13#10+'A web browser should load '+Edit1.Text+#13#10+#13#10;
  Label2.Text:=Label2.Text+'Unfortunately, TWebBrowser has a bug and doesn''t work in a Frame.';

  tmpItem:=FindParentNeTabItem(self);

  if Assigned(tmpItem) then
  begin
    tmpItem.Text:=Edit1.Text;
    tmpItem.TagVariant:=Edit1.Text;
  end;

end;

end.
