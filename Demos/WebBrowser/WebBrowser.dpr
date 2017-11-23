program WebBrowser;

{$R *.dres}

uses
  {$IFDEF EurekaLog}
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
  {$ENDIF EurekaLog}
  System.StartUpCopy,
  FMX.Forms,
  Unit8 in 'Unit8.pas' {Form8},
  Unit2 in 'Unit2.pas' {Frame2: TFrame},
  Unit3 in 'Unit3.pas' {Frame3: TFrame},
  Unit5 in 'Unit5.pas' {Frame5: TFrame},
  neTabTypes in '..\..\SourceCode\Package\neTabTypes.pas',
  neTabItem in '..\..\SourceCode\Package\neTabItem.pas',
  neTabGeneralUtils in '..\..\SourceCode\Package\neTabGeneralUtils.pas',
  neTabControl in '..\..\SourceCode\Package\neTabControl.pas',
  Model.Subscriber in '..\..\SourceCode\SupportCode\Model.Subscriber.pas',
  Model.Provider in '..\..\SourceCode\SupportCode\Model.Provider.pas',
  Model.Interf in '..\..\SourceCode\SupportCode\Model.Interf.pas',
  Model.IntActions in '..\..\SourceCode\SupportCode\Model.IntActions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
