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
  MinorVersion = '1';
  BugVersion = '1';


//***************************************************************
//
// Version History
//
//
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
    function GetCloseImage: TImage;
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

    fProvider: IProvider;
    fMouseState: TMouseState;
    fRefreshCloseImage: boolean;
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

    //Procedures for the close image
    procedure CloseImageOnMouseEnter(Sender: TObject);
    procedure CloseImageMouseUp(ASender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure CloseImageMouseDown(ASender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  protected
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X,
      Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyStyle; override;
    /// <summary>
    ///   Refreshes the control by calling ApplyStyle and DoMouseLeave event.
    /// </summary>
    procedure RefreshControl;

    property Provider: IProvider read fProvider;
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
    tmpControl.InnerControl.Align:=TAlignLayout.Left;
    tmpControl.Width:=tmpControl.InnerControl.Width;
    tmpControl.Align:=TAlignLayout.Left;
    tmpControl.Margins.Left:=5;
    tmpControl.Margins.Top:=4;
    tmpControl.Margins.Bottom:=4;
    tmpControl.Margins.Right:=2;
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

  end;


  fRefreshCloseImage:=false;

  //Close image

  tmpImage.Free;

  if fCanClose then
  begin

    tmpImage:=TneCloseImageControl.Create(self);
    case fMouseState of
      msNone: tmpImage.SetCloseImage(fCloseImages[ImageNormal]);
      msHover: tmpImage.SetCloseImage(fCloseImages[ImageHover]);
      msUp: tmpImage.SetCloseImage(fCloseImages[ImageNormal]);
      msDown: tmpImage.SetCloseImage(fCloseImages[ImagePressed]);
    end;

    tmpImage.Align:=TAlignLayout.Right;
    tmpImage.Margins.Right:=5;
    tmpImage.Margins.Top:=4;
    tmpImage.Margins.Bottom:=4;
    tmpImage.Margins.Left:=4;
    tmpImage.CloseImage.OnMouseEnter:=CloseImageOnMouseEnter;
    tmpImage.CloseImage.OnMouseUp:=CloseImageMouseUp;
    tmpImage.CloseImage.OnMouseDown:=CloseImageMouseDown;

    if Assigned(tmpFMXObject) then
        tmpImage.Parent:=tmpFMXObject;
  end;

end;


procedure TneTabItem.CloseImageMouseDown(ASender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  fMouseState:=msDown;
  {TODO -oOwner -cCategory : Access Violation on MacOS}
//////////
///  Access violation on MacOS
///  Need to investigate
/////////
  {$IFDEF MSWINDOWS}
  ApplyStyle;
  {$ENDIF}
end;

procedure TneTabItem.CloseImageMouseUp(ASender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  fMouseState:=msUp;
  {$IFDEF MSWINDOWS}
  ApplyStyle;
  {$ENDIF}
  NotifyToDeleteTab;
end;

procedure TneTabItem.CloseImageOnMouseEnter(Sender: TObject);
begin
  inherited;
  fMouseState:=msHover;
  {$IFDEF MSWINDOWS}
  ApplyStyle;
  {$ENDIF}
end;

procedure TneTabItem.CloseMenuItem(Sender: TObject);
begin
  NotifyToDeleteTab;
end;

constructor TneTabItem.Create(AOwner: TComponent);
begin
  inherited;
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
  AutoSize:=False;
  fMinTabWidth:=110;
  fMaxTabWidth:=160;
  fTabWidth:=fMinTabWidth;
end;

destructor TneTabItem.Destroy;
begin
  fPopupMenuBeforeDefault.Free;
  fPopupMenuAfterDefault.Free;
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
  fMouseState:=msNone;
  fRefreshCloseImage:=True;
  {$IFDEF MSWINDOWS}
  ApplyStyle;
  {$ENDIF}
end;

procedure TneTabItem.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  self.DoMouseLeave;
end;

procedure TneTabItem.SetCanClose(const showCont: Boolean);
begin
  fCanClose:=showCont;
  RefreshControl;
end;

procedure TneTabItem.SetCloseImage(const index: TCloseImagesType;
  const newImage: TBitmap);
begin
  fCloseImages[index]:=newImage;
  fCloseImageToShow:=fCloseImages[ImageNormal];
end;

procedure TneTabItem.SetControlToShow(const newControl: TControl);
begin
  fControlToShow:=newControl;
  ApplyStyle;
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


procedure TneTabItem.RefreshControl;
begin
  fMouseState:=msNone;
  fRefreshCloseImage:=false;
  ApplyStyle;
end;

{ TneCloseImageControl }

constructor TneCloseImageControl.Create(AOwner: TComponent);
begin
  inherited;
  fControl:=TImage.Create(self);
  fControl.Parent:=self;
  fControl.Align:=TAlignLayout.client;
  self.Width:=16;
end;

destructor TneCloseImageControl.Destroy;
begin
  inherited;
end;

function TneCloseImageControl.GetCloseImage: TImage;
begin
  result:=(fControl as TImage);
end;

procedure TneCloseImageControl.SetCloseImage(const newImage: TBitmap);
begin
  (fControl as TImage).Bitmap.Assign(newImage);
  self.Width:=CloseImage.Width;
end;

end.
