object Form2: TForm2
  Left = 0
  Top = 0
  Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1085#1080#1077' '#1044#1047
  ClientHeight = 406
  ClientWidth = 678
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
  object dtpHWEditing: TDateTimePicker
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
    OnChange = dtpHWEditingChange
  end
  object sgDayShedule: TStringGrid
    Left = 48
    Top = 136
    Width = 370
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
  object btnSaveHW: TButton
    Left = 448
    Top = 288
    Width = 201
    Height = 49
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    TabOrder = 2
    OnClick = btnSaveHWClick
  end
end
