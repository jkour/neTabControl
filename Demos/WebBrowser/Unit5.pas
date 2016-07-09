unit Unit5;

interface

{$DEFINE TMS}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, neTabControl
  {$IFDEF TMS}
  , FMX.TMSBadge
  {$ENDIF}
  , FMX.TMSProgressBar;


type
  TFrame5 = class(TFrame)
    Rectangle1: TRectangle;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    ProgressBar1: TProgressBar;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    procedure Button1Click(Sender: TObject);
  private
    fneTabControl: TnetabControl;
    currentControl: TControl;
    checkTimer: TTimer;
    {$IFDEF TMS}
    tnewBadge: TTMSFMXBadge;
    tnewProg: TTMSFMXProgressBar;
    {$ENDIF}
    procedure OnTimerCheck (Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property neTabControl: TneTabControl read fneTabControl write fneTabControl;
  end;

implementation

uses
	System.Threading, neTabItem, FMX.Ani, System.SyncObjs, FMX.TMSLedmeter;

{$R *.fmx}

procedure TFrame5.Button1Click(Sender: TObject);
var
  tItem: TneTabItem;
  tnewControl: TAniIndicator;
  tnewArc: TArcDial;
  tnewRect: TRectangle;
  tmpProg: TProgressBar;
begin
  if not Assigned(neTabControl) then
    Exit;

  tItem:=neTabControl.GetTab(self.TagString);

  if not Assigned(tItem) then
    Exit;

  Button1.Enabled:=false;
  Label3.Text:='Downloading...';

  checkTimer.Enabled:=true;
  currentControl:=neTabControl.GetControl(self.TagString);

  if RadioButton1.IsChecked or RadioButton2.IsChecked
    or RadioButton4.IsChecked then
  begin
    if RadioButton1.IsChecked then
    begin
      tnewControl:=TAniIndicator.Create(tItem);
      tnewControl.Enabled:=true;
      tnewControl.Width:=16;
      tnewControl.Height:=16;

      tItem.ControlToShow:=tnewControl;
    end;

    if RadioButton2.IsChecked then
    begin
      tnewArc:=TArcDial.Create(tItem);
      tnewArc.Value:=0;
      tnewArc.Width:=16;
      tnewArc.Height:=16;

      tItem.ControlToShow:=tnewArc;
    end;

    if RadioButton4.IsChecked then
    begin
      tmpProg:=TProgressBar.create(tItem);
      tmpProg.Width:=32;
      tmpProg.Height:=12;

      tItem.ControlToShow:=tmpProg;
    end;

    ProgressBar1.Value:=0;
    ProgressBar1.Visible:=True;

    TTask.run(procedure
              var
              pro: Integer;
              begin
                pro:=0;
                while pro<100 do
                begin
                  TThread.Synchronize(nil,
                                procedure
                                begin
                                  ProgressBar1.Value:=ProgressBar1.Value + 1.0;
                                  if RadioButton2.IsChecked then
                                  begin
                                    tnewArc.Value:=tnewArc.Value + 10.0;
                                    tItem.RefreshControl;
                                  end
                                  else
                                  if RadioButton4.IsChecked then
                                  begin
                                    tmpProg.Value:=tmpProg.Value + 1.0;
                                    tItem.RefreshControl;
                                  end;
                                end);
                  inc(pro);
                  Sleep(50);
                end;
              end
              );


  end;

{$IFDEF TMS}
  if RadioButton3.IsChecked then
  begin
    tnewRect:=TRectangle.Create(tItem);
    tnewRect.Fill.Color:=$F000000 or $FFFFFFF; // or TAlphaColor(random($FFFFFF));
    tnewRect.Stroke.Color:=$F000000 or $FFFFFFF;
    tnewRect.Width:=30;
    tnewRect.Height:=30;

    if not Assigned(tnewBadge) then
      tnewBadge:=TTMSFMXBadge.Create(tItem);
    tnewBadge.Text:='0';
    tnewBadge.Parent:=tnewRect;
    tnewBadge.Margins.Top:=2;
    tnewBadge.Margins.Left:=2;
    tnewBadge.Margins.Bottom:=2;
    tnewBadge.Margins.Right:=2;

    tnewBadge.Align:=TAlignLayout.Client;

    tItem.ControlToShow:=tnewRect;

    ProgressBar1.Value:=0;
    ProgressBar1.Visible:=True;

    TTask.run(procedure
              var
              pro: Integer;
              begin
                pro:=0;
                while pro<100 do
                begin
                  TThread.Synchronize(nil,
                                procedure
                                begin
                                  ProgressBar1.Value:=ProgressBar1.Value + 1.0;
                                  tnewBadge.Text:=pro.ToString;
                                  tItem.RefreshControl;
                                end);
                  inc(pro);
                  Sleep(50);
                end;
              end
              );

  end;

  if RadioButton5.IsChecked then
  begin
    if not Assigned(tnewProg) then
      tnewProg:=TTMSFMXProgressBar.Create(tItem);
    tnewProg.Width:=32;
    tnewProg.Height:=12;
    tnewProg.ShowText:=True;

    tnewProg.Align:=TAlignLayout.Client;
    tnewProg.Value:=0;

    tItem.ControlToShow:=tnewProg;

    ProgressBar1.Value:=0;
    ProgressBar1.Visible:=True;

    TTask.run(procedure
              var
              pro: Integer;
              begin
                pro:=0;
                while pro<100 do
                begin
                  TThread.Synchronize(nil,
                                procedure
                                begin
                                  ProgressBar1.Value:=ProgressBar1.Value + 1.0;
                                  tnewProg.Value:=tnewProg.Value + 1.0;
                                  tItem.RefreshControl;
                                end);
                  inc(pro);
                  Sleep(50);
                end;
              end
              );

  end;

{$ENDIF}

  GroupBox1.Enabled:=False;
end;

constructor TFrame5.Create(AOwner: TComponent);
begin
  inherited;
  Self.Name:='';
  {$IFDEF TMS}
    RadioButton3.Enabled:=true;
    RadioButton5.Enabled:=true;
  {$ELSE}
    RadioButton3.Enabled:=false;
    RadioButton5.Enabled:=False;
  {$ENDIF}
  checkTimer:=TTimer.Create(self);
  checkTimer.Enabled:=false;
  checkTimer.Interval:=1;
  checkTimer.OnTimer:=OnTimerCheck;
  ProgressBar1.Visible:=false;
end;

destructor TFrame5.Destroy;
begin
  checkTimer.Free;
  inherited;
end;

procedure TFrame5.OnTimerCheck(Sender: TObject);
var
  tItem: TneTabItem;
begin
  if ProgressBar1.Value=ProgressBar1.Max then
  begin
    checkTimer.Enabled:=False;
    Label3.Text:='Ready to download';
    if Assigned(neTabControl) and Assigned(currentControl) then
    begin
      tItem:=neTabControl.GetTab(self.TagString);

      if Assigned(tItem) then
      begin
        tItem.ControlToShow:=currentControl;
      end;
    end;
    GroupBox1.Enabled:=true;
    ProgressBar1.Visible:=false;
    Button1.Enabled:=true;
  end;

end;

end.
