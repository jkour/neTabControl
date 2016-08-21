/// <exclude />
unit Model.Provider;

interface

uses Model.IntActions, Model.Interf,
    System.Generics.Collections;

function ProviderClass: IProvider;

implementation

type
 TProvider = class (TInterfacedObject, IProvider)
  private
    /// <summary>
    ///   This list keeps track of the subscribers the provider is managing
    /// </summary>
    /// <seealso cref="Model.ProSu.Interfaces|ISubscriberInterface">
    ///   Isubscriber
    /// </seealso>
    fSubscriberList: TList<ISubscriber>;
  public
    procedure Subscribe(tmpSubscriber: ISubscriber);
    procedure Unsubscribe(tmpSubscriber: ISubscriber);
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
    procedure NotifySubscribers (action: TIntActions); overload;
    procedure NotifySubscribers (notificationClass: INotification); overload;

    constructor Create;
    destructor Destroy; override;
  end;

function ProviderClass: IProvider;
begin
  result:=TProvider.Create;
end;

{ TProSuProvider }

constructor TProvider.Create;
begin
  inherited;
  fSubscriberList:=TList<ISubscriber>.Create;
end;

destructor TProvider.Destroy;
var
  iTemp: ISubscriber;
begin
  for itemp in fSubscriberList do
      Unsubscribe(iTemp);
  fSubscriberList.Free;
  inherited;
end;

procedure TProvider.NotifySubscribers(
  notificationClass: INotification);
var
  tmpSubscriber: ISubscriber;
begin
  for tmpSubscriber in fSubscriberList  do
      tmpSubscriber.UpdateSubscriber(notificationClass);
end;

procedure TProvider.NotifySubscribers(action: TIntActions);
var
  tmpSubscriber: ISubscriber;
begin
  for tmpSubscriber in fSubscriberList  do
    tmpSubscriber.UpdateSubscriber(action);
end;

procedure TProvider.Subscribe(tmpSubscriber: ISubscriber);
begin
  fSubscriberList.Add(tmpSubscriber);
end;

procedure TProvider.Unsubscribe(tmpSubscriber: ISubscriber);
begin
  fSubscriberList.Remove(tmpSubscriber);
end;

end.
