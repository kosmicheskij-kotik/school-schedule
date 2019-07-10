object frmUserComplaints: TfrmUserComplaints
  Left = 0
  Top = 0
  Caption = #1056#1077#1076#1072#1082#1090#1086#1088' '#1079#1072#1084#1077#1095#1072#1085#1080#1081
  ClientHeight = 317
  ClientWidth = 575
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lblChooseStudent: TLabel
    Left = 336
    Top = 21
    Width = 112
    Height = 13
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1091#1095#1072#1097#1077#1075#1086#1089#1103':'
  end
  object lblCompList: TLabel
    Left = 48
    Top = 80
    Width = 96
    Height = 13
    Caption = #1057#1087#1080#1089#1086#1082' '#1079#1072#1084#1077#1095#1072#1085#1080#1081':'
  end
  object lblEditComp: TLabel
    Left = 336
    Top = 80
    Width = 105
    Height = 13
    Caption = #1056#1077#1076#1072#1082#1090#1086#1088' '#1079#1072#1084#1077#1095#1072#1085#1080#1081
  end
  object lblCurrentWeek: TLabel
    Left = 104
    Top = 8
    Width = 74
    Height = 13
    Caption = 'lblSelectedDate'
  end
  object cbbStudents: TComboBox
    Left = 336
    Top = 40
    Width = 177
    Height = 28
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Open Sans'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = cbbStudentsChange
  end
  object edtAddComplaint: TEdit
    Left = 336
    Top = 96
    Width = 177
    Height = 21
    TabOrder = 1
  end
  object btnSave: TButton
    Left = 336
    Top = 200
    Width = 177
    Height = 41
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    TabOrder = 2
    OnClick = btnSaveClick
  end
  object btnRemove: TButton
    Left = 336
    Top = 247
    Width = 177
    Height = 41
    Caption = #1059#1076#1072#1083#1080#1090#1100
    TabOrder = 3
    OnClick = btnRemoveClick
  end
  object btnPrevWeek: TButton
    Left = 48
    Top = 40
    Width = 75
    Height = 34
    Caption = #1053#1072#1079#1072#1076
    TabOrder = 4
    OnClick = btnPrevWeekClick
  end
  object btnNextWeek: TButton
    Left = 129
    Top = 40
    Width = 75
    Height = 34
    Caption = #1042#1087#1077#1088#1105#1076
    TabOrder = 5
    OnClick = btnNextWeekClick
  end
  object btnCurrentWeek: TButton
    Left = 208
    Top = 40
    Width = 89
    Height = 34
    Caption = #1058#1077#1082#1091#1097#1072#1103' '#1085#1077#1076#1077#1083#1103
    TabOrder = 6
    OnClick = btnCurrentWeekClick
  end
  object lvComplaints: TListView
    Left = 47
    Top = 96
    Width = 250
    Height = 192
    Columns = <
      item
        AutoSize = True
        Caption = 'Item'
      end>
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    TabOrder = 7
    ViewStyle = vsReport
    OnClick = lvComplaintsClick
  end
end
