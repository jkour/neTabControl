program neTabControlDemo;

{$R *.dres}

uses
  {$IFDEF EUREKALOG}
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
  {$ENDIF }
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  neTabControl in 'neTabControl.pas',
  Unit1 in 'Unit1.pas' {Frame1: TFrame},
  Unit3 in 'Unit3.pas' {Frame3: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
