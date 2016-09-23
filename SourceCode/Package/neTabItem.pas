//***************************************************************
// This source is written by John Kouraklis.
// © 2016, John Kouraklis
// Email : j_kour@hotmail.com
//
// The MIT License (MIT)
//
// Copyright (c) 2016 John Kouraklis
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
// KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
// COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// Unit Name: neTabItem
//
//
//
//

unit neTabItem;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts,
  FMX.Styles.Objects, FMX.StdCtrls,FMX.TabControl, System.Generics.Collections,
  FMX.Forms, FMX.Graphics, FMX.Objects, System.UITypes, Model.Provider,
  Model.Interf, Model.Subscriber, Model.IntActions,
  neTabTypes, neTabGeneralUtils, FMX.Menus, System.Types;

const
  MajorVersion = '1';
  MinorVersion = '3';
  BugVersion = '0';


//***************************************************************
//
// Version History
//
//
// 1.3.0 - 24/09/2016
//
//** New Features
//
//    * ControlAlignment, CaptionAlignment, CloseAlignment properties added
//    * Access to the text element of a TabItem
//    * TabPosition property added (see below for the renaming of the old
//      TabPosition property)
//
//** Improvement
//
//    * The TabPosition property in previous releases is renamed to
//      TabOrientation
//
// 1.2.0 - 21/08/2016
//
//** Bug
//    * Access Violation when the mouse hovered over close image on MacOS
//      and Win64 (This fixed a known issue in 1.1.1)
//    * fActiveTab was never set
//
//** New Features
//
//    * GetFrame method added
//    * OnBeforeCloseAllItems, OnAfterCloseAllItems, OnBeforeCloseOtherItems,
//      OnAfterCloseOtherItems events added
//    * DeleteAllTabs procedure added
//    * Tabs[index] property added
//
// 1.1.1 - 09/07/2016
//
//** Improvement
//
//    * Observer framework renamed
//
// 1.1.0 - 09/07/2016
//
//** Improvement
//
//    * Installer recognises Delphi installation and targets
//
// 1.0.0 - 15/06/2016
//
// Note: This is a code-break update. The neTabControl is not based solely
// on the TabControl as in the previous versions
// The component is rewritten from scratch to provide new features
//
//** New Features
//    * Option to place the tab items at the top and the left side of the control
//    * Option to add buttons before or after the tab items
//    * Style-independent tab items: The tab items don't need a modified style
//      to hold the close image and the other controls
//    * You can embed any type of control (TControl descendent) in a tabitem
//      and not only an image as in previous versions
//
// 0.3.0 - 28/03/2016
//
//** New Features
//    * DeleteAllTabs procedure added.
//    * Method to find the parent neTabItem of a frame
//    * TagValue property added to neTabItem
//    * ActiveTag property Added
//    * neTabItem has an Identifier property
//    * Add ability to save tabs
//
//** Improvement
//    * Code clean up
//    * Title of Tab and Preview appear as Hint
//    * Spelling mistake: MixTabWidth instead of MinTabWidth
//
//**Bug
//    * Before/After popup menus didn't trigger OnClick events
//
//
// 0.2.0 - 05/03/2016
//
//** Changes
//    * Change license to MIT
//    * ShowHint disabled
//    * Check for duplicate Tag name added
//    * Adjust Width improvement
//
//** Bug
//    * Access Violation on MouseOverTabItem when the control is
//      host in other control (eg. a Frame)
//    * AdjustWidth returns 0 when the control is within two
//      frames
//
//** New Feature
//    * Option to disable right-click menu added
//    * MinWidth property added
//    * Normal TTabItem can be added
//    * Add "Close all the rest" option in popup menu
//
//
// 0.1.0 - Initial Version (20/02/2016)


type

  TCloseImagesType = (ImageNormal, ImageHover, ImagePressed);
  TMouseState = (msNone, msHover, msUp, msDown);

  TneControl = class (TControl)
  private
    fControl: TControl;
  public
    property InnerControl: TControl read fControl write fControl;
  end;

  TneCloseImageControl = class (TneControl)
  private
    fProvider: IProvider;
    function GetCloseImage: TImage;
    procedure NotifyTabItem(const newAction: TIntAction);
  protected
    procedure DoMouseLeave; override;
    procedure DoMouseEnter; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    property CloseImage: TImage read GetCloseImage;
    procedure SetCloseImage(const newImage: TBitmap);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   This class represents a tabitem in the control
  /// </summary>
  [ComponentPlatforms(pidWin32 or pidWin64 or pidOSX32)]
  TneTabItem = class(TTabItem)
  private
    //fields
    fTabTag: string;
    fShowControl: Boolean;
    fCanClose: Boolean;
    fControlToShow: TControl;
    fCloseImageToShow: TBitmap;
    fCloseImages : array[ImageNormal..ImagePressed] of TBitmap;
    fShowPopupMenu: Boolean;
    fPopupMenuBeforeDefault,
    fPopupMenuAfterDefault: TPopupMenu;
    fTabWidth: single;
    fMinTabWidth,
    fMaxTabWidth: Single;
    fControlAlignment,
    fCaptionAlignment,
    fCloseAlignment: TAlignLayout;

    fLastMousePoint: TPointF;

    fProvider: IProvider;
    fSubscriber: ISubscriber;
    fMouseState: TMouseState;
    fRefreshCloseImage: boolean;
    fInternalTimer: TneTimer;
    fCaption: TLabel;
    fHintTimer: TneHintTimer;
    fHintInterval: Cardinal;
    fHintText: string;
    //procedures/functions
    procedure SetControlToShow (const newControl: TControl);
    procedure SetCloseImage(const index: TCloseImagesType; const newImage: TBitmap);
    procedure SetShowControl (const showCont: Boolean);
    procedure SetCanClose (const showCont: Boolean);
    function GetCloseImage(const index: TCloseImagesType): TBitmap;
    procedure NotifyToDeleteTab;
    procedure ShowFullPopupMenu(Sender: TObject; X, Y: Single);
    procedure CloseMenuItem (Sender: TObject);
    procedure SetTabWidth (const newWidth: single);
    procedure SetMinTabWidth (const newWidth: single);
    procedure SetMaxTabWidth (const newWidth: single);
    function GetTabWidth: Single;
    procedure SafeApplyStyle;
    procedure OnInternalTimer (Sender: TObject);
    procedure SetControlAlignment(const Value: TAlignLayout);
    procedure SetCaptionAlignment(const Value: TAlignLayout);
    procedure SetCloseAlignment(const Value: TalignLayout);
    procedure SetCaption(const newCaption: TLabel);

    procedure OnHintTimer(Sender:TObject);

  protected
    procedure UpdateFromProvider(const notificationClass: INotification);

    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X,
      Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure SetWidth(const Value: Single); override;
  published
    /// <summary>
    ///   This property holds the tag of the tab item (identifier)
    /// </summary>
    property TabTag: string read fTabTag write fTabTag;
    /// <summary>
    ///   Shows the control at the left side of the tab
    /// </summary>
    property ShowControl: boolean read fShowControl write setShowControl default true;
    /// <summary>
    ///   If true the tab can close and the close button is shown at the right
    ///   side of the tab
    /// </summary>
    property CanClose: boolean read fCanClose write SetCanClose default true;
    /// <summary>
    ///   This property defines the control that appears in the tab item.
    /// </summary>
    /// <remarks>
    ///   Show and hide the control by changing the <see cref="neTabControl|TneTabItem.ShowControl">
    ///   ShowControl</see> property
    /// </remarks>
    property ControlToShow: TControl read fControlToShow write SetControlToShow;
    property CloseImageNormal: TBitmap index ImageNormal read GetCloseImage write SetCloseImage;
    property CloseImageHover: TBitmap index ImageHover read GetCloseImage write SetCloseImage;
    property CloseImagePressed: TBitmap index ImagePressed read GetCloseImage write SetCloseImage;
    property ShowPopupMenu: boolean read fShowPopupMenu write fShowPopupMenu;
    property PopupMenuBeforeDefault: TPopupMenu read fPopupMenuBeforeDefault
      write fPopupMenuBeforeDefault;
    property PopupMenuAfterDefault: TPopupMenu read fPopupMenuAfterDefault
      write fPopupMenuAfterDefault;

    property TabWidth: single read GetTabWidth write SetTabWidth;
    property MinTabWidth: single read fMinTabWidth write SetMinTabWidth;
    property MaxTabWidth: single read fMaxTabWidth write SetMaxTabWidth;
    {$REGION 'Changes the alignment of the Control in the tabitem'}
    /// <summary>
    ///   Changes the alignment of the Control in the tabitem
    /// </summary>
    {$ENDREGION}
    property ControlAlignment: TAlignLayout read fControlAlignment write SetControlAlignment;
    {$REGION 'Changes the alignment of the caption (text) of the tabitem'}
    /// <summary>
    ///   Changes the alignment of the caption (text) of the tabitem
    /// </summary>
    {$ENDREGION}
    property CaptionAlignment: TAlignLayout read fCaptionAlignment write SetCaptionAlignment;
    {$REGION 'Changes the alignment of the close image'}
    /// <summary>
    ///   Changes the alignment of the close image
    /// </summary>
    {$ENDREGION}
    property CloseAlignment: TAlignLayout read fCloseAlignment write SetCloseAlignment;

    property HintInterval: cardinal read fHintInterval write fHintInterval default 1000;
    {$REGION 'Defines the text to show as hint.'}
    /// <summary>
    ///   Defines the text to show as hint.
    /// </summary>
    /// <remarks>
    ///   <list type="number">
    ///     <item>
    ///       Works only if <see cref="neTabControl|TneTabControl.HintType">
    ///       HintType</see> is Text
    ///     </item>
    ///     <item>
    ///       If blank, it shows the title of the tab
    ///     </item>
    ///   </list>
    /// </remarks>
    {$ENDREGION}
    property HintText: string read fHintText write fHintText;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   Refreshes the control by calling ApplyStyle and DoMouseLeave event.
    /// </summary>
    procedure RefreshControl;
    procedure ApplyStyle; override;

    property Provider: IProvider read fProvider;
    {$REGION 'Provides access to the ''text'' element of a tab item'}
    /// <summary>
    ///   Provides access to the 'text' element of a tab item
    /// </summary>
    {$ENDREGION}
    property Caption: TLabel read fCaption write SetCaption;
  end;


procedure register;

implementation

uses
	System.Math;

procedure Register;
begin
  RegisterComponents('NusEnvision', [TneTabItem]);
end;

{ TneTabItem }

procedure TneTabItem.ApplyStyle;
var
  tmpFMXObject, tmpFMXObject1: TFmxObject;
  tmpControl: TneControl;
  tmpImage: TneCloseImageControl;
  tmpBitmap: TBitmap;
  tmpCaption: TFMXObject;
  tmpCaptionControl: TText;
begin
  inherited;

  tmpImage:=nil;
  tmpControl:=nil;

  //Get parent control
  tmpFMXObject:=FindStyleResource('top');

  //Retrieve the controls from the tab item
  if Assigned(tmpFMXObject) then
  begin
    for tmpFMXObject1 in tmpFMXObject.Children do
    begin
      if tmpFMXObject1.ClassNameIs('TneControl') then
        tmpControl:=tmpFMXObject1 as TneControl;
      if tmpFMXObject1.ClassNameIs('TneCloseImageControl') then
        tmpImage:=tmpFMXObject1 as TneCloseImageControl;
    end;
  end;

  //Control
  if not fShowControl then
    tmpControl.Free;

  if Assigned(fControlToShow) and fShowControl and (not fRefreshCloseImage) then
  begin
    tmpControl.Free;

    tmpControl:=TneControl.Create(self);
    if Assigned(tmpFMXObject) then
        tmpControl.Parent:=tmpFMXObject;

    tmpControl.InnerControl:=fControlToShow.Clone(self) as TControl;
    tmpControl.InnerControl.Parent:=tmpControl;
    tmpControl.Width:=tmpControl.InnerControl.Width;
    tmpControl.HitTest:=fControlToShow.HitTest;

    tmpControl.Margins.Left:=fControlToShow.Margins.Left;
    tmpControl.Margins.Top:=fControlToShow.Margins.Top;
    tmpControl.Margins.Bottom:=fControlToShow.Margins.Bottom;
    tmpControl.Margins.Right:=fControlToShow.Margins.Right;

    tmpControl.InnerControl.TagString:=fControlToShow.TagString;

    tmpControl.InnerControl.OnClick:=fControlToShow.OnClick;
    tmpControl.InnerControl.OnDblClick:=fControlToShow.OnDblClick;
    tmpControl.InnerControl.OnEnter:=fControlToShow.OnEnter;
    tmpControl.InnerControl.OnExit:=fControlToShow.OnExit;
    tmpControl.InnerControl.OnMouseDown:=fControlToShow.OnMouseDown;
    tmpControl.InnerControl.OnMouseUp:=fControlToShow.OnMouseUp;
    tmpControl.InnerControl.OnMouseLeave:=fControlToShow.OnMouseLeave;
    tmpControl.InnerControl.OnMouseEnter:=fControlToShow.OnMouseEnter;
    tmpControl.InnerControl.OnMouseMove:=fControlToShow.OnMouseMove;
    tmpControl.InnerControl.Align:=TAlignLayout.Center;
    tmpControl.InnerControl.Margins.Top:=0;
    tmpControl.InnerControl.Margins.Bottom:=0;
    tmpControl.InnerControl.Margins.Left:=0;
    tmpControl.InnerControl.Margins.Right:=0;
  end;

  //Close image

  fRefreshCloseImage:=false;

  if Assigned(tmpImage) then
  begin
    tmpImage.fProvider.Unsubscribe(fSubscriber);
    tmpImage.Free;
  end;

  if fCanClose then
  begin
    tmpImage:=TneCloseImageControl.Create(self);
    tmpImage.fProvider.Subscribe(fSubscriber);

    case fMouseState of
      msNone: tmpBitmap:=fCloseImages[ImageNormal];
      msHover: tmpBitmap:=fCloseImages[ImageHover];
      msUp: tmpBitmap:=fCloseImages[ImageNormal];
      msDown: tmpBitmap:=fCloseImages[ImagePressed];
    end;

    if Assigned(tmpBitmap) then
      tmpImage.SetCloseImage(tmpBitmap);

    tmpImage.Margins.Right:=5;
    tmpImage.Margins.Top:=4;
    tmpImage.Margins.Bottom:=4;
    tmpImage.Margins.Left:=5;

    if Assigned(tmpFMXObject) then
      tmpImage.Parent:=tmpFMXObject;
  end;

  if Assigned(tmpControl) then
    tmpControl.Align:=fControlAlignment;

  tmpCaption:=FindTextObject;
  if Assigned(tmpCaption) then
  begin
    tmpCaptionControl:=tmpCaption as TText;
    tmpCaptionControl.Align:=fCaptionAlignment;
    tmpCaptionControl.Margins:=fCaption.Margins;
    tmpCaptionControl.TextSettings:=fCaption.TextSettings;
  end;

  if Assigned(tmpImage) then
    tmpImage.Align:=fCloseAlignment;
end;


procedure TneTabItem.CloseMenuItem(Sender: TObject);
begin
  NotifyToDeleteTab;
end;

constructor TneTabItem.Create(AOwner: TComponent);
begin
  inherited;
  AutoSize:=false;
  fTabTag:='';
  fShowControl:=true;
  fCanClose:=true;
  fCloseImages[ImageNormal]:=nil;
  fCloseImages[ImageHover]:=nil;
  fCloseImages[ImagePressed]:=nil;
  fShowPopupMenu:=true;
  fPopupMenuBeforeDefault:=nil;
  fPopupMenuAfterDefault:=nil;
  fProvider:=ProviderClass;
  fMouseState:=msNone;
  fRefreshCloseImage:=false;
  fMinTabWidth:=110;
  fMaxTabWidth:=160;
  fTabWidth:=fMinTabWidth;
  fControlAlignment:=TAlignLayout.Left;
  fCaptionAlignment:=TalignLayout.Client;
  fCloseAlignment:=TAlignLayout.Right;

  fSubscriber:=SubscriberClass;
  fSubscriber.SetUpdateSubscriberMethod(UpdateFromProvider);

  fInternalTimer:=TneTimer.Create(self);
  fInternalTimer.Interval:=10;
  fInternalTimer.OnTimer:=OnInternalTimer;
  fInternalTimer.Enabled:=false;

  fCaption:=TLabel.Create(self);
  fCaption.TextSettings.HorzAlign:=TTextAlign.Center;

  fHintInterval:=1000;
  fHintTimer:=TneHintTimer.Create(self);
  fHintTimer.Interval:=fHintInterval;
  fHintTimer.Enabled:=true;
  fHintTimer.OnTimer:=OnHintTimer;
end;

destructor TneTabItem.Destroy;
begin
  fSubscriber:=nil;
  fProvider:=nil;
  fPopupMenuBeforeDefault.Free;
  fPopupMenuAfterDefault.Free;
  fInternalTimer.Free;
  fCaption.Free;
  fHintTimer.Free;
  inherited;
end;

function TneTabItem.GetCloseImage(const index: TCloseImagesType): TBitmap;
begin
  result:=fCloseImages[index];
end;

function TneTabItem.GetTabWidth: Single;
begin
  result:=fTabWidth;
end;

procedure TneTabItem.MouseClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  if Button=TMouseButton.mbRight then
    Self.ShowFullPopupMenu(self,X,Y);
end;

procedure TneTabItem.DoMouseLeave;
begin
  inherited;
  fHintTimer.Enabled:=false;
end;

procedure TneTabItem.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  fMouseState:=msNone;
  fRefreshCloseImage:=True;
  if (abs(fLastMousePoint.X-X)<4) and (Abs(fLastMousePoint.Y-Y)<4) then
  begin
    fHintTimer.MousePoint:=TPointF.Create(X,Y);
    fHintTimer.Action:=intactShowHint;
    fHintTimer.Enabled:=true;
  end
  else
  begin
    fLastMousePoint.X:=X;
    fLastMousePoint.Y:=Y;
  end;
  SafeApplyStyle;
end;

procedure TneTabItem.SafeApplyStyle;
begin
  ApplyStyle;
end;

procedure TneTabItem.SetCanClose(const showCont: Boolean);
begin
  fCanClose:=showCont;
  RefreshControl;
end;

procedure TneTabItem.SetCaption(const newCaption: TLabel);
begin
  fCaption:=newCaption;
end;

procedure TneTabItem.SetCaptionAlignment(const Value: TAlignLayout);
begin
  fCaptionAlignment := Value;
  SafeApplyStyle;
end;

procedure TneTabItem.SetCloseAlignment(const Value: TalignLayout);
begin
  fCloseAlignment := Value;
  SafeApplyStyle;
end;

procedure TneTabItem.SetCloseImage(const index: TCloseImagesType;
  const newImage: TBitmap);
begin
  fCloseImages[index]:=newImage;
  fCloseImageToShow:=fCloseImages[ImageNormal];
end;

procedure TneTabItem.SetControlAlignment(const Value: TAlignLayout);
begin
  fControlAlignment := Value;
  SafeApplyStyle;
end;

procedure TneTabItem.SetControlToShow(const newControl: TControl);
begin
  fControlToShow:=newControl;
  SafeApplyStyle;
end;


procedure TneTabItem.SetMaxTabWidth(const newWidth: single);
begin
  fMaxTabWidth:=newWidth;
  SetTabWidth(self.Width);
end;

procedure TneTabItem.SetMinTabWidth(const newWidth: single);
begin
  fMinTabWidth:=newWidth;
  SetTabWidth(self.Width);
end;

procedure TneTabItem.SetShowControl(const showCont: Boolean);
begin
  fShowControl:=showCont;
  RefreshControl;
end;

procedure TneTabItem.SetTabWidth(const newWidth: single);
var
  nWidth: Single;
begin
  nWidth:=Max(newWidth, fMinTabWidth);
  nWidth:=Min(nWidth, fMaxTabWidth);
  self.Width:=nWidth;
  fTabWidth:=nWidth;
end;

procedure TneTabItem.SetWidth(const Value: Single);
begin
  inherited;
  SetSize(max(fMinTabWidth, Value), FSize.Height, False);
end;

procedure TneTabItem.ShowFullPopupMenu(Sender: TObject; X, Y: Single);
var
  tmpPop: TPopupMenu;
  tmpMenuItem: TMenuItem;
  notifClass: TneNotificationClass;
begin
  if not fShowPopupMenu then
    Exit;

  tmpPop:=TPopupMenu.Create(self);

  if fCanClose then
  begin
    tmpMenuItem:=TMenuItem.Create(tmpPop);
    tmpMenuItem.Parent:=tmpPop;
    tmpMenuItem.Text:=SCloseTab;
    tmpMenuItem.OnClick:=CloseMenuItem;
  end;

  notifClass:=TneNotificationClass.Create;
  notifClass.Sender:=self;
  notifClass.Value:=self.TabTag;
  notifClass.Action:=[TIntAction.intactShowPopupMenu];
  notifClass.Point:=TPointF.Create(X,Y);
  notifClass.PopupBefore:=fPopupMenuBeforeDefault;
  notifClass.PopupDefault:=tmpPop;
  notifClass.PopupAfter:=fPopupMenuAfterDefault;

  fProvider.NotifySubscribers(notifClass);

end;

procedure TneTabItem.UpdateFromProvider(const notificationClass: INotification);
var
  tmpActions: TIntActions;
begin
  if not Assigned(notificationClass) then
    Exit;
  tmpActions:=(notificationClass as TneNotificationClass).Action;
  if intactMouseEnter in tmpActions then
    fMouseState:=msHover;
  if intactMouseLeave in tmpActions then
    fMouseState:=msNone;
  if intactMouseUp in tmpActions then
    fMouseState:=msUp;
  if intactMouseDown in tmpActions then
    fMouseState:=msDown;
  fInternalTimer.Enabled:=true;
  if intactMouseUp in tmpActions then
    NotifyToDeleteTab;
end;

procedure TneTabItem.NotifyToDeleteTab;
var
  newNotifClass: TneNotificationClass;
begin
  newNotifClass:=TneNotificationClass.Create;
  newNotifClass.Action:=[intactDeleteTab];
  newNotifClass.Sender:=Self;
  newNotifClass.Value:=self.TabTag;
  fProvider.NotifySubscribers(newNotifClass);
end;

procedure TneTabItem.OnHintTimer(Sender: TObject);
var
  tmpNotifClass: TneNotificationClass;
begin
  fHintTimer.Enabled:=false;
  tmpNotifClass:=TneNotificationClass.Create;
  if fHintTimer.Action=intactShowHint then
    tmpNotifClass.Action:=[intactShowHint]
  else
    tmpNotifClass.Action:=[intactHideHint];
  tmpNotifClass.Sender:=fHintTimer;
  tmpNotifClass.Value:=self.TabTag;
  tmpNotifClass.Point:=fHintTimer.MousePoint;
  fProvider.NotifySubscribers(tmpNotifClass);
end;

procedure TneTabItem.OnInternalTimer(Sender: TObject);
begin
  fInternalTimer.Enabled:=false;
  SafeApplyStyle;
end;

procedure TneTabItem.RefreshControl;
begin
  fMouseState:=msNone;
  fRefreshCloseImage:=false;
  fInternalTimer.Enabled:=true;
end;

{ TneCloseImageControl }

constructor TneCloseImageControl.Create(AOwner: TComponent);
begin
  inherited;
  fControl:=TImage.Create(self);
  fControl.Parent:=self;
  fControl.Align:=TAlignLayout.client;
  fControl.HitTest:=false;
  self.Width:=16;
  fProvider:=ProviderClass;
end;

procedure TneCloseImageControl.DoMouseLeave;
begin
  inherited;
  NotifyTabItem(intactMouseLeave);
end;

destructor TneCloseImageControl.Destroy;
begin
  fProvider:=nil;
  inherited;
end;

procedure TneCloseImageControl.DoMouseEnter;
begin
  inherited;
  NotifyTabItem(intactMouseEnter);
end;

function TneCloseImageControl.GetCloseImage: TImage;
begin
  result:=(fControl as TImage);
end;

procedure TneCloseImageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  NotifyTabItem(intactMouseDown);
end;

procedure TneCloseImageControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  NotifyTabItem(intactMouseUp);
end;

procedure TneCloseImageControl.NotifyTabItem(const newAction: TIntAction);
var
  newNotifClass: TneNotificationClass;
begin
  inherited;
  newNotifClass:=TneNotificationClass.Create;
  newNotifClass.Action:=[newAction];
  newNotifClass.Sender:=Self;
  fProvider.NotifySubscribers(newNotifClass);
end;

procedure TneCloseImageControl.SetCloseImage(const newImage: TBitmap);
begin
  (fControl as TImage).Bitmap.Assign(newImage);
  self.Width:=CloseImage.Width;
end;

end.
