program neTabControlWithinFrames;

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
  Unit4 in 'Unit4.pas' {Form4},
  Unit3 in '..\..\Unit3.pas' {Frame3: TFrame},
  Unit5 in 'Unit5.pas' {BasicFrame: TFrame},
  neTabControl in '..\..\neTabControl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
