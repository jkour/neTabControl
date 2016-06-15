unit Unit3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, neTabControl;

type
  TFrame3 = class(TFrame)
    Rectangle1: TRectangle;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
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

{ TFrame3 }

procedure TFrame3.CheckBox1Change(Sender: TObject);
begin
  if not Assigned(fParentTabControl) then
    Exit;

  if CheckBox1.IsChecked then
    fParentTabControl.GetTab(self.TagString).ShowControl:=true
  else
    fParentTabControl.GetTab(self.TagString).ShowControl:=false;

end;

procedure TFrame3.CheckBox2Change(Sender: TObject);
begin
  if not Assigned(fParentTabControl) then
    Exit;

  if CheckBox2.IsChecked then
    fParentTabControl.GetTab(self.TagString).ShowPopupMenu:=true
  else
    fParentTabControl.GetTab(self.TagString).ShowPopupMenu:=false;

end;

procedure TFrame3.CheckBox3Change(Sender: TObject);
begin
  if not Assigned(fParentTabControl) then
    Exit;

  if CheckBox3.IsChecked then
    fParentTabControl.GetTab(self.TagString).CanClose:=true
  else
    fParentTabControl.GetTab(self.TagString).CanClose:=false;

end;

constructor TFrame3.Create(AOwner: TComponent);
begin
  inherited;
  self.Name:='';
end;

procedure TFrame3.SetParentTabControl(const newParent: TneTabControl);
begin
  fParentTabControl:=newParent;
  self.Align:=TAlignLayout.Client;
end;

end.
