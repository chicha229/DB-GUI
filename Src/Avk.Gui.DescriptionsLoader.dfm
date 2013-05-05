object DescriptionsLoaderDM: TDescriptionsLoaderDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 424
  Width = 586
  object BlocksQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 40
    Top = 8
  end
  object ProceduresQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 40
    Top = 56
  end
  object FormsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 40
    Top = 104
  end
  object BlockParamsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 104
    Top = 24
  end
  object BlockActionsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 104
    Top = 72
  end
  object ActionBindsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 104
    Top = 120
  end
  object FormChildsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 40
    Top = 152
  end
  object FormChildParamsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 104
    Top = 168
  end
  object FormPanelsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 40
    Top = 208
  end
  object BlockRefParamsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 104
    Top = 216
  end
  object BlockRefBindsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 40
    Top = 256
  end
  object ChildRefBindsQuery: TADQuery
    ConnectionName = 'MainConnection'
    Left = 40
    Top = 304
  end
end
