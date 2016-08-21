program TabControlDesignerProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  TabControlDesigner in 'TabControlDesigner.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
