/// <exclude />
/// <summary>
///   This unit defines the actions that can be communicated
/// </summary>
/// <remarks>
///   Use
/// </remarks>
unit Model.ProSu.InterfaceActions;

interface

type

  /// <summary>
  ///   An enumerator describing the actions that can be transmitted to
  ///   subscribers
  /// </summary>
  /// <remarks>
  ///   Add more options in this type to meet the needs in your application
  /// </remarks>
  TInterfaceAction = (
    /// <summary>
    ///   This action is the default. It may lead to no action
    /// </summary>
    Default,
    DeleteTab,
    ShowPopupMenu,
    UpdateTabHeight
    );

  {$REGION 'The set of actions to be passed to the Observers'}
  /// <summary>
  ///   The set of actions to be passed to the Observers
  /// </summary>
  /// <example>
  ///   NotifySubscribers([UpdateListBox, UpdateLabel])
  /// </example>
  /// <seealso cref="Model.ProSu.InterfaceActions|TInterfaceAction" />
  {$ENDREGION}
  TInterfaceActions = set of TInterfaceAction;

implementation

end.
