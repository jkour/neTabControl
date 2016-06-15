program WebBrowser;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit2 in 'Unit2.pas' {Frame2: TFrame},
  Unit3 in 'Unit3.pas' {Frame3: TFrame},
  Unit5 in 'Unit5.pas' {Frame5: TFrame},
  Unit8 in 'Unit8.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
