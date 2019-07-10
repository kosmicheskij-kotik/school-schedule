unit Auth;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TAccess = (aTeacher, aParent, aStudent);
  TpwData = packed record
    Password: String[20];
    LogIn: String[20];
    UserName: String[50];
    Access: TAccess;
  end;
  PPwList = ^TpwList;
  TPwList = packed record
    Data: TpwData;
    mNext: PPwList;
  end;

  TfrmAuthorization = class(TForm)
    edLogin: TEdit;
    lblLogin: TLabel;
    lblPassword: TLabel;
    edPassword: TEdit;
    btnEnter: TBitBtn;
    btnExit: TBitBtn;
    lblWrong: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure btnEnterClick(Sender: TObject);
    procedure edLoginClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
  private
    function CheckLogIn(var UsersList: PpwList; EditLogin, EditPassword: TEdit): Boolean;
  public
    procedure SavePasswordToFile(Sender: TObject; var UsersList: PpwList; const FileName: String);
  end;

var
  frmAuthorization: TfrmAuthorization;
  AccessRight: TAccess;
  CurrentUser: TpwData;

implementation

uses
  UsersUnit, MainShedule;
{$R *.dfm}

procedure TfrmAuthorization.btnEnterClick(Sender: TObject);
begin
  if CheckLogIn(UsersList, edLogin, edPassword) then
  begin
    Close;
    frmAuthorization.ModalResult := mrOk;
    if AnsiUpperCase(edLogin.Text) = AnsiUpperCase(MainUser) then
      AccessRight := aTeacher
  end
  else
    lblWrong.Caption := 'Неверный логин/пароль'
end;

procedure TfrmAuthorization.btnExitClick(Sender: TObject);
begin
  Close;
end;

function TfrmAuthorization.CheckLogIn(var UsersList: PpwList; EditLogin, EditPassword: TEdit): Boolean;
var
  P: PpwList;
  isUserFound: boolean;
begin
  Result := False;
  P := UsersList;
  isUserFound := false;
  while Assigned(P) and not isUserFound do
  begin
    if (AnsiUpperCase(String(P^.Data.LogIn)) = (AnsiUpperCase(EditLogin.Text))) and
      (P^.Data.Password = ShortString(EditPassword.Text)) then
    begin
      Result := True;
      isUserFound := true;
      with CurrentUser do
      begin
        LogIn := P^.Data.LogIn;
        Password := P^.Data.Password;
        UserName := P^.Data.UserName;
        Access := P^.Data.Access;
      end;
    end;
    P := P^.mNext;
  end;
end;

procedure TfrmAuthorization.edLoginClick(Sender: TObject);
begin
  lblWrong.Caption := '';
end;

procedure TfrmAuthorization.FormActivate(Sender: TObject);
begin
  lblWrong.Caption := '';
  UsersList := nil;
  pwListInitializtion(UsersList);
end;

procedure TfrmAuthorization.SavePasswordToFile(Sender: TObject; var UsersList: PpwList;
  const FileName: String);
var
  F: File of TpwData;
  P: PpwList;
begin
  AssignFile(F,FileName);
  Rewrite(F);
  P := UsersList;
  while Assigned(P) do
  begin
    write(F, P^.Data);
    P := P^.mNext
  end;
  CloseFile(F);
end;

end.
