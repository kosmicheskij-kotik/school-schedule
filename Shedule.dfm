object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #1042#1080#1088#1090#1091#1072#1083#1100#1085#1099#1081' '#1076#1085#1077#1074#1085#1080#1082
  ClientHeight = 700
  ClientWidth = 1000
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblNameOfMonths: TLabel
    Left = 424
    Top = 8
    Width = 142
    Height = 22
    Caption = 'lblNameOfMonths'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Open Sans SemiBold'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblWeekDate: TLabel
    Left = 408
    Top = 36
    Width = 75
    Height = 18
    Caption = 'lblWeekDate'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Open Sans'
    Font.Style = []
    ParentFont = False
  end
  object sgMonday: TStringGrid
    Left = 72
    Top = 72
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 9
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 0
  end
  object sgTuesday: TStringGrid
    Left = 72
    Top = 281
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 9
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 1
  end
  object sgWednesDay: TStringGrid
    Left = 72
    Top = 490
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 9
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 2
  end
  object sgThursDay: TStringGrid
    Left = 464
    Top = 72
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 9
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 3
  end
  object sgFriday: TStringGrid
    Left = 464
    Top = 281
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 9
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 4
  end
  object sgSaturday: TStringGrid
    Left = 464
    Top = 490
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 9
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 5
  end
end
