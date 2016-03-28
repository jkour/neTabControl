unit Unit4;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  View.Options.Template, FMX.TabControl, FMX.TMSBaseControl, FMX.TMSTableView,
  FMX.Objects, FMX.TMSHTMLText, FMX.Layouts;

type
  TOptionsTemplateView4 = class(TOptionsTemplateView)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsTemplateView4: TOptionsTemplateView4;

implementation

{$R *.fmx}

uses Unit5;

procedure TOptionsTemplateView4.FormCreate(Sender: TObject);
var
  tmpClass: TOptionsClass;
  tmpFrame: TFrame5;
begin
  inherited;
  tmpClass:=TOptionsClass.Create;
  tmpClass.Name:='New';
  tmpClass.Description:='New frame';

  tmpFrame:=TFrame5.Create(nil);
  tmpFrame.Setup;
  self.AddOption('New',tmpClass, tmpFrame);
  self.SetSelection('New');

end;

end.
