unit UsersUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls, Auth,
  Vcl.Buttons;

type
  TfrmUsers = class(TForm)
    listboxUsers: TListBox;
    lbLogin: TLabel;
    lbPassword: TLabel;
    edLogin: TEdit;
    edPassword: TEdit;
    lbUsers: TLabel;
    btnApply: TBitBtn;
    btnDeleteUser: TBitBtn;
    rgAccess: TRadioGroup;
    lblUserName: TLabel;
    edUserName: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure listboxUsersClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnDeleteUserClick(Sender: TObject);
  private
    procedure LoadList(var pHead: PpwList; var ListBox: TListBox);
    procedure FindUser(var pHead: PpwList; const ListBox: TListBox;
      var EditLogin, EditUserName: TEdit);
    procedure AddCheckedData(var pHead: PpwList; const Edit1,Edit2, Edit3: TEdit);
    procedure AddUser(var pHead: PpwList; const Data: TpwData);
    procedure DeleteUser(var pHead: PpwList; const Edit1: TEdit);
    procedure ClearFields(var Edit1, Edit2, Edit3: TEdit; var ListBox: TListBox);
  public
    { Public declarations }
  end;
const
  msgUserExist = 'Пользователь уже существует.' + #13 + #10 +
    'Изменить пароль?';
  msgConfirm = 'Подтверждение';
  MainUser = 'admin';
  msgMainUser = 'Невозможно удалить пользователя';
  msgConfirmDeleteUser = 'Вы действительно ходите удалить пользователя ';
  msgNotEnoughRigths = 'Недостаточно прав для данного действия.';
  msgErr = 'Ошибка';
var
  frmUsers: TfrmUsers;

implementation
{$R *.dfm}
uses
  MainShedule;
procedure TfrmUsers.listboxUsersClick(Sender: TObject);
begin
  if listboxUsers.ItemIndex <> -1 then
    FindUser(UsersList,listboxUsers,edLogin, edUserName);
end;

procedure TfrmUsers.LoadList(var pHead: PpwList; var ListBox: TListBox);
var
  P: PpwList;
begin
  P := pHead;
  while Assigned(P) do
  begin
    ListBox.Items.Add(String(P^.Data.LogIn));
    P := P^.mNext;
  end;
end;

procedure TfrmUsers.AddUser(var pHead: PpwList; const Data: TpwData);
var
  P: PpwList;
begin
  New(P);
  P^.Data.Password := Data.Password;
  P^.Data.LogIn := Data.LogIn;
  P^.Data.UserName := Data.UserName;
  case rgAccess.ItemIndex of
    0: P^.Data.Access := aTeacher;
    1: P^.Data.Access := aStudent;
    2: P^.Data.Access := aParent;
  end;
  P^.mNext := UsersList;
  UsersList := P;
end;

procedure TfrmUsers.btnApplyClick(Sender: TObject);
begin
  if AccessRight = aTeacher then
  begin
    AddCheckedData(UsersList,edLogIn,EdPassword, edUserName);
    listboxUsers.Clear;
    LoadList(UsersList,listboxUsers);
    ClearFields(edLogIn, edPassword, edUserName, listboxUsers);
  end
  else
  begin
    MessageBox(Handle,msgNotEnoughRigths,msgErr,MB_OK+MB_ICONERROR);
  end;
end;

procedure TfrmUsers.btnDeleteUserClick(Sender: TObject);
begin
  if edLogin.Text <> MainUser then
  begin
    if
    (MessageBox(handle,
      PWideChar(msgConfirmDeleteUser + Trim(edLogin.Text) + '?'),
      msgConfirm, 
      mb_YesNO or mb_DEFBUTTON2 or MB_ICONQUESTION) = IDYES)
    then
    begin
      DeleteUser(UsersList,edLogin);
      listboxUsers.Clear;
      LoadList(UsersList,listboxUsers);
      ClearFields(edLogIn, edPassword, edUserName, listboxUsers);
    end;
  end
  else
    MessageBox(Handle,msgMainUser + ' ' + MainUser,msgErr,MB_OK+MB_ICONERROR);
end;

procedure TfrmUsers.ClearFields(var Edit1, Edit2, Edit3: TEdit; var ListBox: TListBox);
begin
  Edit1.Clear;
  Edit2.Clear;
  Edit3.Clear;
  ListBox.ItemIndex := 0;
end;

procedure TfrmUsers.AddCheckedData(var pHead: PpwList; const Edit1, Edit2, Edit3: TEdit);
var
  P: PpwList;
  UserExist: Boolean;
  UserData: TpwData;
begin
  P := pHead;
  UserExist := False;
  while Assigned(P) and not UserExist do
  begin
    if Edit1.Text = String(P^.Data.LogIn) then
    begin
      if (MessageBox(handle, msgUserExist, msgConfirm,
      mb_YesNO or mb_DEFBUTTON2 or MB_ICONQUESTION) = IDYes) then
      begin
        P^.Data.Password := ShortString(Edit2.Text);
        P^.Data.UserName := ShortString(Edit3.Text);
      end;
      UserExist := True;
    end;
    P := P^.mNext
  end;
  if not(UserExist) then
  begin
    UserData.LogIn := ShortString(Edit1.Text);
    UserData.Password := ShortString(Edit2.Text);
    UserData.UserName := ShortString(Edit3.Text);
    AddUser(pHead,UserData);
  end;
end;

procedure TfrmUsers.DeleteUser(var pHead: PpwList; const Edit1: TEdit);
var
  P: PpwList;
begin
  P := pHead;
  if P^.Data.LogIn = ShortString(Edit1.Text) then
    pHead := P^.mNext;
  while Assigned(P) and Assigned(P^.mNext) do
  begin
    if P^.mNext^.Data.LogIn = ShortString(Edit1.Text) then
    begin
      P^.mNext := P^.mNext^.mNext;
    end;
    P := P^.mNext;
  end;
end;

procedure TfrmUsers.FindUser(var pHead: PpwList; const ListBox: TListBox;
  var EditLogin, EditUserName: TEdit);
var
  P: PpwList;
begin
  P := pHead;
  while Assigned(P) do
  begin
    if ListBox.Items[ListBox.ItemIndex] = String(P^.Data.LogIn) then
    begin
      EditLogin.Text := String(P^.Data.LogIn);
      EditUserName.Text := String(P^.Data.UserName);
    end;
    P := P^.mNext;
  end;
end;

procedure TfrmUsers.FormActivate(Sender: TObject);
begin
  LoadList(UsersList,listboxUsers);
end;

procedure TfrmUsers.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  frmAuthorization.SavePasswordToFile(Sender, UsersList, 'users.pw');
  listboxUsers.Clear;
  edPassword.Text := '';
  edLogIn.Text := '';
end;

end.
