object fmrMain: TfmrMain
  Left = 457
  Top = 250
  Caption = 'fmrMain'
  ClientHeight = 844
  ClientWidth = 1249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 287
    Top = 0
    Width = 5
    Height = 844
  end
  object RenderPanel: TPanel
    Left = 292
    Top = 0
    Width = 957
    Height = 844
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnDblClick = RenderPanelDblClick
    OnMouseMove = RenderPanelMouseMove
  end
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 287
    Height = 844
    Align = alLeft
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 1
      Top = 824
      Width = 285
      Height = 5
      Cursor = crVSplit
      Align = alBottom
    end
    object lbObjects: TListBox
      Left = 1
      Top = 1
      Width = 285
      Height = 823
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 64
      TabOrder = 0
      OnDblClick = lbObjectsDblClick
      OnDrawItem = lbObjectsDrawItem
    end
    object Panel1: TPanel
      Left = 1
      Top = 829
      Width = 285
      Height = 14
      Align = alBottom
      Caption = 'Panel1'
      TabOrder = 1
    end
  end
  object MainMenu: TMainMenu
    Left = 304
    Top = 80
    object MenuItem1: TMenuItem
      Caption = 'File'
      object miNew: TMenuItem
        Caption = 'New'
        OnClick = miNewClick
      end
      object miOpen: TMenuItem
        Caption = 'Open'
        OnClick = miOpenClick
      end
      object miSave: TMenuItem
        Caption = 'Save'
        OnClick = miSaveClick
      end
    end
  end
  object SaveRoomDialog: TSaveDialog
    DefaultExt = '.room'
    Filter = 'Room|*.room'
    Left = 439
    Top = 76
  end
  object OpenRoomDialog: TOpenDialog
    DefaultExt = '.room'
    Filter = 'Room|*.room'
    Left = 439
    Top = 128
  end
end
