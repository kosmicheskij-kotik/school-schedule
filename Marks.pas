unit Marks;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Grids, Auth;

type
  PMarks = ^TMarksList;
  TMark = record
    Date: TDateTime;
    SubjectID: Integer;
    NumOfSubject: Integer;
    Value: String[2];
  end;
  TMarksList = record
    Mark: TMark;
    NextMark: PMarks;
  end;

  TForm4 = class(TForm)
    lblChoosedate: TLabel;
    sgDayMarkShedule: TStringGrid;
    lblShedule: TLabel;
    btnSaveMarks: TButton;
    dtpMarkEditing: TDateTimePicker;
    cbbStudents: TComboBox;
    lblChooseStudent: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveMarksClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure LoadToCbb(Sender: TObject);
    procedure dtpMarkEditingChange(Sender: TObject);
    procedure cbbStudentsChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure LoadMarksToSG(Sender: TObject; sgDayMarkShedule: TStringGrid; CurrentLogin: string; CurrentDate: TDateTime; MarksList: PMarks);
  end;
var
  Form4: TForm4;
  MarksList: PMarks;
  StudentsList: PPwList;

 function GetMarkByDate(const MarkDate: TDateTime; NumOfSubject: integer; var MarksList: PMarks): string;
 procedure LoadMarksFromFile(const UserName: string; var MarksList: PMarks);
 procedure AddStudent(var pHead: PpwList; const Data: TpwData);
 function getLoginByID(Id: integer; StudentsList: PPwList):string;

implementation
{$R *.dfm}
uses
  MainShedule;

var
  CurrentDate : TDateTime;
  CurrentLogin: string;

procedure DisposeEl(LastMark: PMarks);
var
  temp : PMarks;
begin
  temp := LastMark;
  if LastMark^.NextMark <> nil then
    LastMark^.NextMark := LastMark^.NextMark.NextMark;
  Dispose(temp);
end;

procedure DisposeStruct(var PList: PPwList);
var
  P: PPwList;
  Temp: PPwList;
begin
  P := PList;
  while Assigned(P) do
  begin
    Temp := P;
    P := P^.mNext;
    Dispose(Temp);
  end;
end;

procedure DisposeMarksStruct(var PList: PMarks);
var
  P: PMarks;
  Temp: PMarks;
begin
  P := PList;
  while Assigned(P) do
  begin
    Temp := P;
    P := P^.NextMark;
    Dispose(Temp);
  end;
end;
procedure AddMark(const MarkDate: TDateTime; NumOfSubject: integer; Value: Shortstring;
                var MarksList: PMarks);
var
  P: PMarks;
begin
  New(P);
  P^.Mark.Date := MarkDate;
  P^.Mark.NumOfSubject := NumOfSubject;
  P^.Mark.SubjectID := getSubjectIDbyDateNum(MarkDate, NumOfSubject, FirstSubject);
  P^.Mark.Value := Value;
  P^.NextMark := MarksList;
  MarksList := P;
end;

function GetMarkByDate(const MarkDate: TDateTime; NumOfSubject: integer; var MarksList: PMarks): string;
var
  LastMark: PMarks;
  isMarkFound : boolean;
begin
  LastMark := MarksList;
  isMarkFound := false;
  while (Assigned(LastMark)) and (not isMarkFound) do
  begin
    if (LastMark^.Mark.Date = MarkDate) and 
       (LastMark^.Mark.NumOfSubject = NumOfSubject) then
    begin
      isMarkFound := true;
      Result := LastMark^.Mark.Value;
    end;
    LastMark := LastMark^.NextMark;
  end;
  if Not isMarkFound then
    Result := '';
end;

procedure EditMarkByDate(const MarkDate: TDateTime; NumOfSubject: integer; Value: String; var MarksList: PMarks);
var
  LastMark: PMarks;
  isMarkFound: boolean;
begin
  LastMark := MarksList;
  isMarkFound := false;
  while (Assigned(LastMark)) and (not isMarkFound) do
  begin
    if (LastMark^.Mark.Date = MarkDate) and 
       (LastMark^.Mark.NumOfSubject = NumOfSubject) then
    begin
      isMarkFound := true;
      if Length(Value) <> 0 then
        LastMark^.Mark.Value := Value
      else
        DisposeEl(LastMark);
    end;
    LastMark := LastMark^.NextMark;
  end;
end;

procedure LoadMarksFromFile(const UserName: string; var MarksList: PMarks);
var
  MarksFile: File of TMark;
  NewMark: TMark;
  P: PMarks;
begin
  P := MarksList;

  if not DirectoryExists(getCurrentDir + '\Marks') then
    ForceDirectories(getCurrentDir + '\Marks');

  if FileExists('Marks\'+ UserName + '.dat') then
  begin
    AssignFile(MarksFile, 'Marks\'+ UserName + '.dat');
    Reset(MarksFile);
    while not EoF(MarksFile) do
    begin
      read(MarksFile, NewMark);
      AddMark(NewMark.Date, NewMark.NumOfSubject, NewMark.Value, MarksList);
    end;
    CloseFile(MarksFile);
  end;
end;

procedure SaveMarksToFile(UserName: string; var MarksList: PMarks);
var
  MarksFile: File of TMark;
  CurrentMark : PMarks;
begin
  CurrentMark := MarksList;
  AssignFile(MarksFile, 'Marks\'+ UserName + '.dat');
  Rewrite(MarksFile);
  while Assigned(CurrentMark) do
  begin
    if Length(CurrentMark^.Mark.Value) <> 0 then
      write(MarksFile, CurrentMark^.Mark)
    else
      disposeEl(CurrentMark);
    CurrentMark := CurrentMark^.NextMark;
  end;
  CloseFile(MarksFile);
end;

 procedure AddStudent(var pHead: PpwList; const Data: TpwData);
var
  P: PpwList;
begin
  New(P);
  P^.Data.Password := Data.Password;
  P^.Data.LogIn := Data.LogIn;
  P^.Data.UserName := Data.UserName;
  P^.Data.Access := Data.Access;
  P^.mNext := pHead;
  pHead := P;
end;

procedure TForm4.LoadToCbb(Sender: TObject);
var
  P: PPwList;
begin
  P := StudentsList;
  while Assigned(P) do
  begin
    cbbStudents.AddItem(P^.Data.UserName, Sender);
    P := P^.mNext;
  end;
end;

function getLoginByID(Id: integer; StudentsList: PPwList):string;
var
  P: PPwList;
  CurrentID : Integer;
begin
  CurrentID := 0;
  P := StudentsList;
  while Assigned(P) and (Id < CurrentID) do
  begin
    P := P^.mNext;
    Inc(CurrentID);
  end;
  if Id = CurrentID then
  begin
    Result := P^.Data.LogIn;
  end;
end;
procedure TForm4.btnSaveMarksClick(Sender: TObject);
var
  MarkValue: string;
  Row: integer;
  DayOfMonday: TDateTime;
  isMarksCorrect: Boolean;
begin
  isMarksCorrect := True;
  for Row := 1 to 7 do
  begin
    with sgDayMarkShedule do
    begin
      if (Length(Cells[0, Row]) < 5) and (Length(Cells[1, Row]) > 0) then
        isMarksCorrect := false;
    end;
    if isMarksCorrect then
    begin
      MarkValue := GetMarkByDate(dtpMarkEditing.Date, Row, MarksList);
      if Length(MarkValue) <> 0 then
        EditMarkByDate(dtpMarkEditing.Date, Row, sgDayMarkShedule.Cells[1,Row], MarksList)
      else
      begin
        if Length(sgDayMarkShedule.Cells[1, Row]) <> 0 then
          AddMark(dtpMarkEditing.Date, Row, sgDayMarkShedule.Cells[1,Row], MarksList);
      end;
    end;
  end;
  if isMarksCorrect then
  begin
    SaveMarksToFile(CurrentLogin, MarksList);
    DayOfMonday := getDateOfMonday(Date);
    Form3.LoadSheduletosg(Sender, DayOfMonday);
    ShowMessage('Успешно сохранено!');
  end
  else
    ShowMessage('Отметки выставлены неверно!');
end;

procedure TForm4.LoadMarksToSG(Sender: TObject; sgDayMarkShedule: TStringGrid; CurrentLogin: string; CurrentDate: TDateTime; MarksList: PMarks);
var
  DayOfWeekValue: integer;
  i: integer;
begin
  DayOfWeekValue := DayOfWeek(CurrentDate) - 1;

  if (DayOfWeekValue <> 0) then
  begin
    with sgDayMarkShedule do
    begin
      for i := 1 to 7 do
      begin
        if (Length(CurrentLogin) > 0) then
          Cells[1,i] := GetMarkByDate(CurrentDate, i, MarksList)
        else
          Cells[1,i] := '';
      end;
    end
  end
  else
  begin
    ShowMessage(MsgIsWeekend);
  end;
end;

procedure TForm4.dtpMarkEditingChange(Sender: TObject);
var
  DayOfWeekValue: integer;
  i : Integer;
begin
  DayOfWeekValue := DayOfWeek(dtpMarkEditing.Date) - 1;
  if DayOfWeekValue <> 0 then
    with sgDayMarkShedule do
    begin
      Cells[0,0] := getDayNameByDate(dtpMarkEditing.Date);
      for i := 1 to 7 do
      begin
        Cells[0,i] := InttoStr(i) + '. ' + getSubjectNameByDate(dtpMarkEditing.Date, i);
      end;
      LoadMarksToSG(Sender, sgDayMarkShedule, CurrentLogin, dtpMarkEditing.Date, MarksList);
    end
  else
  begin
    ShowMessage(MsgIsWeekend);
  end;
end;

procedure TForm4.cbbStudentsChange(Sender: TObject);
begin
  CurrentLogin := getLoginByID(cbbStudents.ItemIndex, StudentsList);
  DisposeMarksStruct(MarksList);
  if (Length(CurrentLogin) > 0) then
      LoadMarksFromFile(CurrentLogin, MarksList);
  LoadMarksToSG(Sender, sgDayMarkShedule, CurrentLogin, dtpMarkEditing.Date, MarksList);
end;

procedure TForm4.FormActivate(Sender: TObject);
begin
  MarksList := nil;
  cbbStudents.Clear;
  dtpMarkEditingChange(Sender);
  if not Assigned(StudentsList) then
    getStudentsList(StudentsList);
  LoadToCbb(Sender);
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  CurrentDate := Date;
  with sgDayMarkShedule do
  begin
    ColWidths[0] := 130;
    ColWidths[1] := 70;
    Cells[1,0] := CMark;
  end;
  dtpMarkEditing.Date := Date;
end;

end.
