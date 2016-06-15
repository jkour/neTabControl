unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  neTabControl, FMX.WebBrowser, FMX.StdCtrls, FMX.Edit,
  FMX.Controls.Presentation, FMX.Layouts;

type
  TFrame2 = class(TFrame)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Layout1: TLayout;
    Layout2: TLayout;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    fParentTabControl: TneTabControl;
    procedure SetParentTabControl (const newParent: TneTabControl);
  public
    property ParentTabControl: TneTabControl read fParentTabControl
      write SetPArentTabControl;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

{ TFrame2 }

procedure TFrame2.Button1Click(Sender: TObject);
begin
  if edit1.text<>'' then
  begin
 //   WebBrowser1.URL:=Edit1.text;
    if Assigned(fParentTabControl) then
    begin
      fParentTabControl.GetTab(self.TagString).Text:=Edit1.Text;
    end;
    Label3.Text:='Normally a webbrowser component would open '+#13+#10+Edit1.text+
      #13+#10+' but, unfortuantely, it does work correctly when put in a frame';
  end;
end;

constructor TFrame2.Create(AOwner: TComponent);
begin
  inherited;
  self.Name:='';
end;

procedure TFrame2.SetParentTabControl(const newParent: TneTabControl);
begin
  fParentTabControl:=newParent;
  self.Align:=TAlignLayout.Client;
end;

end.
