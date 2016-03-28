program neTabControlDemo;

{$R *.dres}

uses
  EMemLeaks,
  EResLeaks,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  EDebugExports,
  EDebugJCL,
  EFixSafeCallException,
  EMapWin32,
  EAppFMX,
  ExceptionLog7,
  System.StartUpCopy,
  FMX.Forms,
  DemoUnit1 in 'DemoUnit1.pas' {Form2},
  Unit3 in 'Unit3.pas' {Frame3: TFrame},
  Unit1 in 'Unit1.pas' {Frame1: TFrame},
  neTabControl in '..\..\neTabControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
