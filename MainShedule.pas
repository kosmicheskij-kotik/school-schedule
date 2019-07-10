unit MainShedule;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls, Auth;

type
  TForm3 = class(TForm)
  	lblNameOfMonths: TLabel;
    lblWeekDate: TLabel;
    sgMonday: TStringGrid;
    sgTuesday: TStringGrid;
    sgWednesDay: TStringGrid;
    sgThursDay: TStringGrid;
    sgFriday: TStringGrid;
    sgSaturday: TStringGrid;
    pnlMainInfoBtns: TPanel;
    btnEditHW: TButton;
    btnEditUserList: TButton;
    lblUserName: TLabel;
    btnEditMarks: TButton;
    btnPrevWeek: TButton;
    btnNextWeek: TButton;
    btnCurrentWeek: TButton;
    btnComplaints: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnEditHWClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnEditUserListClick(Sender: TObject);
    procedure btnEditMarksClick(Sender: TObject);
    procedure btnCurrentWeekClick(Sender: TObject);
    procedure btnPrevWeekClick(Sender: TObject);
    procedure btnNextWeekClick(Sender: TObject);
    procedure btnComplaintsClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure LoadSheduletosg(Sender: TObject; DateOfMonday: TDateTime);
  end;

  PSubjectList = ^TSubjectList;
  TSubjects = record
    ID: integer;
    Name: String[25];
  end;
  TSubjectList = record
    Subject: TSubjects;
    NextSubject: PSubjectList;
  end;

  TShedule = array[1..6] of array [1..7] of integer;

  TDayOfWeek = record
    Monday, TuesDay,
    Wednesday, ThursDay,
    Friday, SaturDay: string;
  end;

var
  UsersList: PPwList;
  StudentsList: PPwList;
  Form3: TForm3;
  SheduleList: TShedule;
  FirstSubject: PSubjectList;
  isUserAuthorized : Boolean;
  CurrentMonday: TDateTime;

const
  CHomeWork: string = 'Домашнее задание';
  CMark: string = 'Отметка';
  CDayOfWeek : TDayOfWeek = (
    Monday: 'Понедельник';
    TuesDay:  'Вторник';
    WednesDay:  'Среда';
    ThursDay:  'Четверг';
    Friday:  'Пятница';
    SaturDay:  'Суббота');
  MsgIsWeekend: string = 'Выбранный день - выходной. Пожалуйста, выберите другую дату';

function getSubjectNameByDate(SubjDate: TDateTime; NumOfSubject: Integer):string;
function getSubjectbyId(id: integer):string;
function getDayNameByDate(DayDate: TDateTime): string;
function getDateOfMonday(CurrentDate: TDateTime):TDateTime;
procedure pwListInitializtion(var UsersList: PPwList);
procedure getStudentsList(var StudentsList: PPwList);
function getSubjectIDbyDateNum(SubjectDate: TDateTime; NumOfSubject: Integer;
                                FirstSubject: PSubjectList):integer;

implementation
uses
  System.DateUtils, HomeWorkEditing, UsersUnit, Marks, UserComplaints;
{$R *.dfm}

function getSubjectNameByDate(SubjDate: TDateTime; NumOfSubject: Integer):string;
var
  DayOfWeekValue: Integer;
begin
  DayOfWeekValue := DayOfWeek(SubjDate) - 1;
  if DayOfWeekValue <> 0 then
    Result := getSubjectbyId(SheduleList[DayOfWeekValue, NumOfSubject])
  else
    Result := 'Weekend';
end;

// Процедура добавления нового предмета в конец списка
procedure addSubject(var Point: PSubjectList; const Name: string; id: integer);
begin
  New(Point^.NextSubject);
  Point := Point^.NextSubject;
  Point^.Subject.Name := name;
  Point^.Subject.id := id;
  Point^.NextSubject := nil;
end;

// Загрузка всех существующих предметов из файла
function loadSubjectsFromFile(var FirstSubject:PSubjectList): integer;
var
  SubjectsFile: File of TSubjects;
  CurrentSubject: PSubjectList;
  NewSubject: TSubjects;
begin
  if FileExists('Subjects.dat') then
  begin
    AssignFile(SubjectsFile, 'Subjects.dat');
    Reset(SubjectsFile);
    CurrentSubject := FirstSubject;
    CurrentSubject^.NextSubject := nil;
    Result := 0;
    While not EoF(SubjectsFile) do
    begin
      read(SubjectsFile, NewSubject);
      addSubject(CurrentSubject, NewSubject.Name, NewSubject.id);
      inc(Result);
    end;
    CloseFile(SubjectsFile);
  end
  else
    Result := 0;
end;

procedure saveSubjectsToFile(FirstSubject:PSubjectList);
var
  SubjectsFile: File of TSubjects;
  CurrentSubject: PSubjectList;
begin
  AssignFile(SubjectsFile, 'Subjects.dat');
  Rewrite(SubjectsFile);
  CurrentSubject := FirstSubject;
  While CurrentSubject^.NextSubject <> nil do
  begin
    CurrentSubject := CurrentSubject^.NextSubject;
    write(SubjectsFile, CurrentSubject^.Subject);
  end;
  CloseFile(SubjectsFile);
end;

function getSubjectIDbyDateNum(SubjectDate: TDateTime; NumOfSubject: Integer;
                                FirstSubject: PSubjectList):integer;
var
  dayWeek: Integer;
begin
  dayWeek := dayOfWeek(SubjectDate)-1;
  if NumOfSubject <= 7 then
    Result := SheduleList[dayWeek, NumOfSubject];
end;
function getSubjectID(Subject: string; FirstSubject: PSubjectList):integer;
var
  CurrentSubject: PSubjectList;
  isSubjectFound: boolean;
  prevSubjectId: integer;
begin

  CurrentSubject := FirstSubject;
  isSubjectFound := false;

  While (CurrentSubject^.NextSubject <> Nil) and (not isSubjectFound) do
  begin
    CurrentSubject := CurrentSubject^.NextSubject;
    if CurrentSubject^.Subject.Name = Subject then
    begin
      Result := CurrentSubject^.Subject.ID;
      isSubjectFound := true;
    end;
  end;
  if not isSubjectFound then
  begin
    prevSubjectId := CurrentSubject^.Subject.Id;
    addSubject(CurrentSubject, Subject, prevSubjectId+1);
    Result := CurrentSubject^.Subject.id;
  end;
  saveSubjectsToFile(FirstSubject);
end;

function getSubjectbyId(id: integer):string;
var
  CurrentSubject: PSubjectList;
  isSubjectFound: boolean;
begin
  CurrentSubject := FirstSubject;
  isSubjectFound := false;
  While (CurrentSubject^.NextSubject <> Nil) and (not isSubjectFound) do
  begin
    CurrentSubject := CurrentSubject^.NextSubject;
    if CurrentSubject^.Subject.Id = id then
    begin
      Result := CurrentSubject^.Subject.Name;
      isSubjectFound := true;
    end;
  end;
  if not isSubjectFound then
    Result := '';
end; 

function getDayNameByDate(DayDate: TDateTime): string;
var
  DayValue : integer;
  MyYear, MyMonths, MyDay: Word;
begin
  DecodeDate(DayDate, MyYear, MyMonths, MyDay);
  DayValue := DayOfWeek(DayDate) - 1;
  case DayValue of
    1: Result := CDayOfWeek.Monday;
    2: Result := CDayOfWeek.TuesDay;
    3: Result := CDayOfWeek.Wednesday;
    4: Result := CDayOfWeek.ThursDay;
    5: Result := CDayOfWeek.Friday;
    6: Result := CDayOfWeek.SaturDay;
  else Result := 'Weekend';
  end;
  Result := Result + ', ' + IntToStr(MyDay);
end;

(* Процедура загрузки расписания из текстового файла
** в типизированный файл
*)
procedure LoadSheduleFromTextFile(var FirstSubject: PSubjectList);
Var
  SheduleTextFile : TextFile;
  SheduleFile: File of TShedule;
  dayOfWeek, NumOfSubject: integer;
  Subject: string;
  SubjectID: integer;
  CurrentSubject: PSubjectList;

begin
  CurrentSubject := FirstSubject;
  CurrentSubject^.NextSubject := nil;
  loadSubjectsFromFile(FirstSubject);
  addSubject(CurrentSubject, '', 0);
  
  AssignFile(SheduleTextFile, 'Shedule.txt');
  AssignFile(SheduleFile, 'Shedule.dat');
  Reset(SheduleTextFile);
  Rewrite(SheduleFile);

  dayOfWeek := 1;
  NumOfSubject := 0;
  While not EoF(SheduleTextFile) and (DayOfWeek < 7) do
  begin
    readln(SheduleTextFile, Subject);
    if Subject <> '' then
    begin
      SubjectID := getSubjectID(Subject, FirstSubject);
      inc(NumOfSubject);
      if NumOfSubject <= 8 then
        SheduleList[dayOfWeek, NumOfSubject] := SubjectID;
    end
    else
    begin
      inc(dayOfWeek);
      NumOfSubject := 0;
    end;
  end;

  write(SheduleFile, SheduleList);
  CloseFile(SheduleTextFile);
  CloseFile(SheduleFile);
end;

procedure pwListInitializtion(var UsersList: PPwList);
var
  StandartUser: TpwData;
  P: PPwList;
  F: File of TpwData;
begin
  AssignFile(F,'users.pw');
  if (FileExists('users.pw')) then
    Reset(F)
  else
  begin
    Rewrite(F);
    StandartUser.LogIn := 'teacher';
    StandartUser.Password := '';
    StandartUser.UserName := 'Учитель';
    write(F,StandartUser);
    CloseFile(F);
    Reset(F);
  end;

  while not (EOF(F)) do
  begin
    New(P);
    Read(F,P^.Data);
    P^.mNext := UsersList;
    UsersList := P;
  end;
  CloseFile(F);
end;

procedure getStudentsList(var StudentsList: PPwList);
var
  P: PPwList;
begin
  StudentsList := nil;
  if not Assigned(UsersList) then
  begin
    UsersList := nil;
    pwListInitializtion(UsersList);
  end;
  P := UsersList;
  while Assigned(P) do
  begin
    if P^.Data.Access = aStudent then
      AddStudent(StudentsList, P^.Data);
    P := P^.mNext;
  end;
end;

procedure LoadSheduleFromFile();
var
  SheduleFile : File of TShedule;
begin
  AssignFile(SheduleFile, 'Shedule.dat');
  Reset(SheduleFile);
  Read(SheduleFile, SheduleList);
  CloseFile(SheduleFile);
end;


procedure TForm3.LoadSheduletosg(Sender: TObject; DateOfMonday: TDateTime);
var
  i : integer;
  Subject: string;
  MyYear, MyMonths, MyPrevMonths, MyDay: Word;
  DateOfDay: TDateTime;
  TestValue: String;

  function getMonthsName(Months:Integer):String;
  begin
    case Months of
      1: Result := 'Январь';
      2: Result := 'Февраль';
      3: Result := 'Март';
      4: Result := 'Апрель';
      5: Result := 'Май';
      6: Result := 'Июнь';
      7: Result := 'Июль';
      8: Result := 'Август';
      9: Result := 'Сентябрь';
      10: Result := 'Октябрь';
      11: Result := 'Ноябрь';
      12: Result := 'Декабрь';
    end;
  end;

begin
  DateOfDay := DateOfMonday;
  DecodeDate(DateOfDay, MyYear, MyPrevMonths, MyDay);
  with sgMonday do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Subject := getSubjectbyId(SheduleList[1, i]);
      Cells[0,i] := IntToStr(i) + '. ' + Subject;
      Cells[1,i] := GetHWByDate(DateOfDay, i, FirstHW);
      if CurrentUser.Access = aStudent then
        Cells[2,i] := GetMarkByDate(DateOfDay, i, MarksList);
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  with sgTuesday do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[2, i]);
      Cells[1,i] := GetHWByDate(DateOfDay, i, FirstHW);
      if CurrentUser.Access = aStudent then
        Cells[2,i] := GetMarkByDate(DateOfDay, i, MarksList);
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  with sgWednesDay do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[3, i]);
      Cells[1,i] := GetHWByDate(DateOfDay, i, FirstHW);
      if CurrentUser.Access = aStudent then
        Cells[2,i] := GetMarkByDate(DateOfDay, i, MarksList);
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  with sgThursDay do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[4, i]);
      Cells[1,i] := GetHWByDate(DateOfDay, i, FirstHW);
      if CurrentUser.Access = aStudent then
        Cells[2,i] := GetMarkByDate(DateOfDay, i, MarksList);
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  with sgFriday do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[5, i]);
      Cells[1,i] := GetHWByDate(DateOfDay, i, FirstHW);
      if CurrentUser.Access = aStudent then
        Cells[2,i] := GetMarkByDate(DateOfDay, i, MarksList);
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  DecodeDate(DateOfDay, MyYear, MyMonths, MyDay);
  with sgSaturday do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[6, i]);
      Cells[1,i] := GetHWByDate(DateOfDay, i, FirstHW);
      if CurrentUser.Access = aStudent then
        Cells[2,i] := GetMarkByDate(DateOfDay, i, MarksList);
    end;
  end;
  lblWeekDate.Caption := DateToStr(DateOfMonday) + ' - ' + DateToStr(DateOfDay);
  lblWeekDate.Left := 405 - lblWeekDate.Width div 2;
  if (MyMonths - MyPrevMonths = 0) then
    lblNameOfMonths.Caption := getMonthsName(MyMonths)
  else
    lblNameOfMonths.Caption := getMonthsName(MyPrevMonths) + ' / ' + getMonthsName(MyMonths);
  lblNameOfMonths.Left := 405 - lblNameOfMonths.Width div 2;
end;

function getDateOfMonday(CurrentDate: TDateTime):TDateTime;
var
  CurrentDayOfWeek: Integer;
begin
  CurrentDayOfWeek := DayOfWeek(CurrentDate);
  Result := CurrentDate;
  if (CurrentDayOfWeek >= 2) then
    Result := IncDay(Result, 2-CurrentDayOfWeek)
  else
    Result := IncDay(Result, 1);
end;

procedure TForm3.btnComplaintsClick(Sender: TObject);
begin
  frmUserComplaints.ShowModal;
end;

procedure TForm3.btnCurrentWeekClick(Sender: TObject);
begin
  CurrentMonday := getDateOfMonday(Date);
  LoadSheduletosg(Sender, CurrentMonday);
end;

procedure TForm3.btnEditHWClick(Sender: TObject);
begin
  Form2.Show;
end;

procedure TForm3.btnEditMarksClick(Sender: TObject);
begin
  Form4.ShowModal;
end;

procedure TForm3.btnEditUserListClick(Sender: TObject);
begin
  FrmUsers.ShowModal;
end;

procedure TForm3.btnNextWeekClick(Sender: TObject);
begin
  CurrentMonday := IncDay(CurrentMonday, 7);
  LoadSheduletosg(Sender, CurrentMonday);
end;

procedure TForm3.btnPrevWeekClick(Sender: TObject);
begin
  CurrentMonday := IncDay(CurrentMonday, -7);
  LoadSheduletosg(Sender, CurrentMonday);
end;

procedure TForm3.FormActivate(Sender: TObject);
begin
  if not isUserAuthorized then
  begin
    if FrmAuthorization.ShowModal = mrOk then
    begin
      lblUserName.Visible := true;
    	isUserAuthorized := true;
      lblUserName.Caption := CurrentUser.UserName;
      if CurrentUser.Access = aStudent then
      begin
        MarksList := nil;
        LoadMarksFromFile(CurrentUser.LogIn, MarksList);
        LoadSheduletosg(Sender, CurrentMonday);
      end;
      lblUserName.Left := 135 - lblUserName.Width div 2;
      if CurrentUser.Access = aTeacher then
      begin
        btnEditUserList.Visible := true;
        btnEditMarks.Visible := true;
      end
      else
      begin
        btnEditUserList.Visible := false;
        btnEditMarks.Visible := false;
      end;
    end;
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  New(FirstSubject);
  CurrentMonday := getDateOfMonday(Date);
  LoadHWFromFile(FirstHW, LastHW);
  LoadSheduleFromTextFile(FirstSubject);
  LoadSheduletosg(Sender, CurrentMonday);
  isUserAuthorized := false;
  lblUserName.Visible := false;
  btnEditUserList.Visible := false;
end;

end.
