/// <exclude />
unit Model.ProSu.Classes.Provider;

interface

uses Model.ProSu.InterfaceActions, Model.ProSu.Interfaces,
    System.Generics.Collections;

function ProviderClass: IProviderInterface;

implementation

type
 TProSuProvider = class (TInterfacedObject, IProviderInterface)
  private
    /// <summary>
    ///   This list keeps track of the subscribers the provider is managing
    /// </summary>
    /// <seealso cref="Model.ProSu.Interfaces|ISubscriberInterface">
    ///   Isubscriber
    /// </seealso>
    fSubscriberList: TList<ISubscriberInterface>;
  public
    procedure Subscribe(tmpSubscriber: ISubscriberInterface);
    procedure Unsubscribe(tmpSubscriber: ISubscriberInterface);
    /// <param name="action">
    ///   <see cref="Model.ProSu.InterfaceActions|TInterfaceActions" />
    /// </param>
    /// <remarks>
    ///   You must always provide action options whenever you use the
    ///   NotifySubscribers. See Examples
    /// </remarks>
    /// <example>
    ///   <para>
    ///     NotifySubscribers([]);
    ///   </para>
    ///   <para>
    ///     NotifySubscribers([Default]);
    ///   </para>
    /// </example>
    procedure NotifySubscribers (action: TInterfaceActions); overload;
    procedure NotifySubscribers (notificationClass: INotificationInterface); overload;

    constructor Create;
    destructor Destroy; override;
  end;

function ProviderClass: IProviderInterface;
begin
  result:=TProSuProvider.Create;
end;

{ TProSuProvider }

constructor TProSuProvider.Create;
begin
  inherited;
  fSubscriberList:=TList<ISubscriberInterface>.Create;
end;

destructor TProSuProvider.Destroy;
var
  iTemp: ISubscriberInterface;
begin
  for itemp in fSubscriberList do
      Unsubscribe(iTemp);
  fSubscriberList.Free;
  inherited;
end;

procedure TProSuProvider.NotifySubscribers(
  notificationClass: INotificationInterface);
var
  tmpSubscriber: ISubscriberInterface;
begin
  for tmpSubscriber in fSubscriberList  do
    tmpSubscriber.UpdateSubscriber(notificationClass);
end;

procedure TProSuProvider.NotifySubscribers(action: TInterfaceActions);
var
  tmpSubscriber: ISubscriberInterface;
begin
  for tmpSubscriber in fSubscriberList  do
    tmpSubscriber.UpdateSubscriber(action);
end;

procedure TProSuProvider.Subscribe(tmpSubscriber: ISubscriberInterface);
begin
  fSubscriberList.Add(tmpSubscriber);
end;

procedure TProSuProvider.Unsubscribe(tmpSubscriber: ISubscriberInterface);
begin
  fSubscriberList.Remove(tmpSubscriber);
end;

end.
