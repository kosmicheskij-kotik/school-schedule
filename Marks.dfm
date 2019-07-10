object Form4: TForm4
  Left = 0
  Top = 0
  Caption = #1042#1099#1089#1090#1072#1074#1083#1077#1085#1080#1077' '#1086#1094#1077#1085#1086#1082
  ClientHeight = 364
  ClientWidth = 652
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblChoosedate: TLabel
    Left = 48
    Top = 33
    Width = 91
    Height = 17
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1076#1072#1090#1091':'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Open Sans'
    Font.Style = []
    ParentFont = False
  end
  object lblShedule: TLabel
    Left = 48
    Top = 114
    Width = 62
    Height = 13
    Caption = #1056#1072#1089#1087#1080#1089#1072#1085#1080#1077':'
  end
  object lblChooseStudent: TLabel
    Left = 312
    Top = 37
    Width = 112
    Height = 13
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1091#1095#1072#1097#1077#1075#1086#1089#1103':'
  end
  object dtpMarkEditing: TDateTimePicker
    Left = 48
    Top = 56
    Width = 202
    Height = 28
    Date = 43242.000000000000000000
    Time = 43242.000000000000000000
    DateFormat = dfLong
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Open Sans'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnChange = dtpMarkEditingChange
  end
  object sgDayMarkShedule: TStringGrid
    Left = 48
    Top = 136
    Width = 200
    Height = 203
    ColCount = 2
    RowCount = 8
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs]
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 0
  end
  object btnSaveMarks: TButton
    Left = 432
    Top = 290
    Width = 212
    Height = 49
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    TabOrder = 2
    OnClick = btnSaveMarksClick
  end
  object cbbStudents: TComboBox
    Left = 312
    Top = 56
    Width = 177
    Height = 28
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Open Sans'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnChange = cbbStudentsChange
  end
end
