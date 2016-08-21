/// <exclude />
/// <summary>
///   This unit defines the actions that can be communicated
/// </summary>
/// <remarks>
///   Use
/// </remarks>
unit Model.IntActions;

interface

type

  /// <summary>
  ///   An enumerator describing the actions that can be transmitted to
  ///   subscribers
  /// </summary>
  /// <remarks>
  ///   Add more options in this type to meet the needs in your application
  /// </remarks>
  TIntAction = (
    /// <summary>
    ///   This action is the default. It may lead to no action
    /// </summary>
    intactDefault,
    intactDeleteTab,
    intactShowPopupMenu,
    intactUpdateTabHeight,
    intactMouseEnter,
    intactMouseLeave,
    intactMouseUp,
    intactMouseDown
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
  TIntActions = set of TIntAction;

implementation

end.
