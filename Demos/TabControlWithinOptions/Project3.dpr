program Project3;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit3 in 'Unit3.pas' {Form3},
  View.BaseForm in '..\..\..\..\OptionsViewTemplate\View.BaseForm.pas' {BaseForm},
  View.Frame.Test in '..\..\..\..\OptionsViewTemplate\View.Frame.Test.pas' {FrameTest: TFrame},
  View.TwoBandBaseForm in '..\..\..\..\OptionsViewTemplate\View.TwoBandBaseForm.pas' {TwoBandBaseForm},
  View.Options.Template in '..\..\..\..\OptionsViewTemplate\View.Options.Template.pas' {OptionsTemplateView},
  Unit4 in 'Unit4.pas' {OptionsTemplateView4},
  Unit5 in 'Unit5.pas' {Frame5: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
