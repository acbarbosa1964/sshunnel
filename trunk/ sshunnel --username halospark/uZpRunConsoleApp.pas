{==============================================================================|
| Module: uZpRunConsoleApp                          | v.01.00.01 | 22-Jan-2004 |
|------------------------------------------------------------------------------|
| Author: Zlatko Petkov                             | License: Freeware        |
|------------------------------------------------------------------------------|
| Content: Module with contructions (funcions and objects) for starting        |
|          console applications with redirection for input output              |
|------------------------------------------------------------------------------|
| Version history:                                                             |
| o 01.00.00 / 14-Jan-2004                                                     |
|   [+] Initial verison.                                                       |
| o 01.00.01 / 22-Jan-2004                                                     |
|   [*] The actual execution of the thread is moved from Execute into          |
|       DoExecConsole;                                                         |
|   [*] The Method of the thread IsRunning is modified (GetIsRunning is        |
|       removed).                                                              |
|   [*] Change in notification thread when new text/row is received.           |
|==============================================================================}

unit uZpRunConsoleApp;

{==============================================================================|
| Instructions for use:                                                        |
|   <none>                                                                     |
|------------------------------------------------------------------------------|
| Information:                                                                 |
|                                                                              |
| Based on information from the following modules:                             |
|                                                                              |
|  ConsoleApp.pas:       Martin Lafferty <martinl@prel.co.uk>                  |
|  JvSysComp.pas:        JEDI VCL                                              |
|  RunConsole.pas:       Marin Atanasov                                        |
|                                                                              |
| Additional information (incomplete):                                         |
|                                                                              |
|  Using anonymous pipes to redirect standard input/output of a child process: |
|     http://community.borland.com/article/0,1410,10387,00.html                |
|                                                                              |
|  How to Spawn Console Processes with Redirected Standard Handles:            |
|     http://support.microsoft.com/default.aspx?scid=KB;EN-US;q190351          |
|                                                                              |
| NOTE: In all exmples and articles there is something incomplete and not in   |
|       sync with the documentation.                                           |
|------------------------------------------------------------------------------|
| Posible extensions:                                                          |
| o None known.                                                                |
|==============================================================================}

interface

uses
  Windows, Classes;

//------------------------------------------------------------------------------
// Data types for general use
//------------------------------------------------------------------------------

type
  TZpProcessPriority = (                // Priority of the child process
    ppIdle,                             // Only in background mode
    ppNormal,                           // Normal priority
    ppHigh,                             // Hight priority
    ppRealTime                          // Execution in real time
  );

type
  TZpOnNewTextEvent = procedure(const Sender: TObject;
      const aText: string) of object;
    {: Event, called when new text is received from the child process. }

  TZpOnStatusChange = procedure(const Sender: TObject;
      const aRunning: Boolean) of object;
    {: Event, called when there is a change in the state of the thread. }

//------------------------------------------------------------------------------
// Functions and procedures for direct execution
//------------------------------------------------------------------------------

function ExecuteConsoleApp(const aCommandLine: string;
  const aCurrentDir: string = '';
  const aAppOutput: TStrings = nil;
  const aOnNewText: TZpOnNewTextEvent = nil;
  const aOnNewLine: TZpOnNewTextEvent = nil;
  const aEnvironment: TStrings = nil;
  const aProcessPriority: TZpProcessPriority = ppNormal): Cardinal;
  {: Starts console application (without window).

    Parameters:
    ----------
    aCommandLine
        Command line (name+params) for the console application. It is
        recommended the name to include the full path to prevent unexpected
        errors.

    aCurrentDir
        Current dir for console application. If the parameter is empty string
        for current dir will be considered the dir of the invoking program.

    aAppOutput
        List with all texts returned from the application.

    aOnNewText
        Event, that is called for each text fragment, recevied from the
        application. When called the prameter "Sender" is nil.

    aOnNewLine
        Event, that is called for each new row received from the application.
        When called the prameter "Sender" is nil.

    aEnvironment
        List with environment variable definitions of the console application.
        Elements must be ANSI, not Unicode and in format 'name=value'.

    aProcessPriority
        Priority of execution for the console application.

    Return codes:
    -------------
      ExitCode of the appliccation or MAXDWORD, if the application did not close
    after internal timeout had occured (CTerminationWaitTime).
      In a case of fatal exception, invokes Exception from type EOSError.

    Notes:
    ------
    - If all three parameters for feedback (aAppOutput, aOnNewText, aOnNewLine)
      are missing, the procedure will be executed correctly, but there will be
      no information about the returned from the process messages.
    - After text is received from the console application, first aOnNewText (if
      set) is invoked with the received full text. After that if the text
      contains new lines, for each new line the following is repeated: it will
      be added to aAppOutput (if set) and aOnNewLine (if set) is called.
    - This procedure is not thread-safe! To be called once and wait until it
      finishes!
  }

function WriteToConsoleApp(const aText: string): Boolean;
function WriteLnToConsoleApp(const aText: string): Boolean;
  {: Sends defined text to the invoked by ExecuteConsoleApp() console
     application. Returns True, if the invoke was successfull. }

function StopConsoleApp: Boolean;
  {: Stops "forcebly" invoked by ExecuteConsoleApp() console application.
     Returns True, if the application finished successfuly or if it was stoped
     successfuly, and False if it was unable to stop it. }

//------------------------------------------------------------------------------
// Thread for execution of the console application
//------------------------------------------------------------------------------

type
  TZpExecConsoleAppThread = class(TThread)
  private
    procedure SyncSendNewText;
    procedure SyncSendNewLine;
    procedure SyncStatusChange;
  private
    procedure SetProcessPriority(const Value: TZpProcessPriority);
    function GetUseAppOutput: Boolean;
    procedure SetUseAppOutput(const Value: Boolean);
    procedure SetEnvironment(const Value: TStringList);
    procedure SetOnNewLine(const Value: TZpOnNewTextEvent);
    procedure SetOnNewText(const Value: TZpOnNewTextEvent);
    procedure SetOnStatusChange(const Value: TZpOnStatusChange);
  protected
    FTextBuffer         : string;               // Help buffer for events OnNewXXX
    FIsRunning          : Boolean;              // Help variable for OnStatusChange
    FCommandLine        : string;               // Command line for execution
    FCurrentDir         : string;               // Working dir for the process
    FProcessPriority    : TZpProcessPriority;   // Priority of the process
    FAppOutput          : TStringList;          // Text ooutput of the application
    FEnvironment        : TStringList;          // Environtment of the process
    FStartupInfo        : TStartupInfo;         // Parameters for execution of the process
    FProcessInfo        : TProcessInformation;  // Information for invoked process
    FWriteHandle        : THandle;              // For sending to the process
    FExitCode           : Cardinal;             // Result of the execution
    FLastError          : Integer;              // Code of the last error
    // Events
    FOnNewText          : TZpOnNewTextEvent;    // For each received text
    FOnNewLine          : TZpOnNewTextEvent;    // For each received full line
    FOnStatusChange     : TZpOnStatusChange;    // In change of the state
  protected
    procedure DoChangeStatus(const aIsRunning: Boolean);
    procedure DoSendNewText(const aNewText: string);
    procedure DoSendNewLine(const aNewLine: string);
    procedure ChangeStatus(const aIsRunning: Boolean); virtual;
    procedure SendNewText(const aNewText: string); virtual;
    procedure SendNewLine(const aNewLine: string); virtual;
    procedure DoExecConsole;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Run(const aCommandLine: string;
      const aCurrentDir: string = ''): Boolean;
      {: Invokes defined by aCommandLine console application. Returns True,
         if it was successful. }
    function Write(const aText: string): Boolean;
    function WriteLn(const aLine: string): Boolean;
      {: Sends defined text/row to the console application. Returns True, if
         it was successful. }
    function Stop: Boolean;
      {: Stops "forcebly" console application. }

    property IsRunning: Boolean read FIsRunning;
      {: Shows if there is a running application at the moment. }
    property CommandLine: string read FCommandLine;
      {: Contains the command line of the last invoked application. }
    property ProcessPriority: TZpProcessPriority
        read FProcessPriority write SetProcessPriority;
      {: Shows the priority for execution of the console application. }
    property AppOutput: TStringList read FAppOutput;
      {: Contains the text output of the last invoked application.
         If UseAppOutput = False, then it value is nil. }
    property UseAppOutput: Boolean read GetUseAppOutput write SetUseAppOutput;
      {: Shows if AppOutput is used for collecting the result of the invoked
         application. }
    property Environment: TStringList read FEnvironment write SetEnvironment;
      {: List the elements (variable=value) for the environment of the
         console application. }
    property ExitCode: Cardinal read FExitCode;
      {: ExitCode of the application or MAXDWORD, if the application did not
         finish with success. }
    property LastError: Integer read FLastError;
      {: Code of the last error or 0 when success. }
    property OnNewText: TZpOnNewTextEvent read FOnNewText write SetOnNewText;
      {: Invoked at every text received from the console application. }
    property OnNewLine: TZpOnNewTextEvent read FOnNewLine write SetOnNewLine;
      {: Invoked at every full line received from the console application. }
    property OnStatusChange: TZpOnStatusChange
        read FOnStatusChange write SetOnStatusChange;
      {: Invoked when there is a change in the state of the thread. }
  end;

//------------------------------------------------------------------------------

implementation

uses
  SysUtils, Forms, Math;

//==============================================================================
// Local constructions
//==============================================================================

const
  CProcessPriorities: array[TZpProcessPriority] of DWORD = (
    IDLE_PRIORITY_CLASS,
    NORMAL_PRIORITY_CLASS,
    HIGH_PRIORITY_CLASS,
    REALTIME_PRIORITY_CLASS
  );

  CZpBufferSize         = 2048;         // Size of the work buffer
  CTerminationWaitTime  = 2000;         // In miliseconds. = 2 sec.

//==============================================================================

type
  TZpRWHandles = record
    ReadHandle          : THandle;
    WriteHandle         : THandle;
  end;

type
  TZpRWEHandles = record
    ReadHandle          : THandle;
    WriteHandle         : THandle;
    ErrorHandle         : THandle;
  end;

type
  TZpBuffer = array[0..CZpBufferSize] of Char;

//==============================================================================

var
  grProcessInfo         : TProcessInformation;
  ghWriteHandle         : THandle;

//==============================================================================
// Help functions for general use
//==============================================================================

function SafeCloseHandle(var aHandle: THandle): Boolean;
begin
  if (aHandle <> 0) then
  begin
    Result := CloseHandle(aHandle);
    if Result then
      aHandle := 0;
  end
  else
    Result := True;
end;

//------------------------------------------------------------------------------

function PCharOrNil(const S: AnsiString): PAnsiChar;
begin
  if (Length(S) = 0) then
    Result := nil
  else
    Result := PAnsiChar(S);
end;

//------------------------------------------------------------------------------
{ Converts the list with strings in list with null-terminated strings with
  double null for end. If the list is empty, or nil was send, returns nil.
  The invoking method must free the used memory with FreeMem();

  Copied from JclStrings.StringsToMultiSz with three modifications:
  1) If the sent list is nil, returns nil.
  2) If there is empty string in the list, it will be skipped without error.
  3) The group of arguments was modified.
}
function StringsToMultiSz(const aSource: TStrings): PChar;
var
  I                     : Integer;
  iTotalLength          : Integer;
  P                     : PChar;
  pcDest                : PChar;
begin
  Result := nil;
  if (aSource = nil) or (aSource.Count = 0) then Exit;

  // Calculate the size
  iTotalLength := 0;
  for I := 0 to aSource.Count - 1 do
    if (aSource[I] <> EmptyStr) then
      Inc(iTotalLength, Length(aSource[I]) + SizeOf(AnsiChar));
  // Memory  alocation
  pcDest := AllocMem(iTotalLength + SizeOf(AnsiChar));
  // Copy the elements from the list
  P := pcDest;
  for I := 0 to aSource.Count - 1 do
  begin
    if (aSource[I] <> EmptyStr) then
    begin
      P := StrECopy(P, PChar(aSource[I]));
      Inc(P);
    end;
  end;
  Result := pcDest;
end;

//==============================================================================
// Help functions, related to module's contructions
//==============================================================================

procedure CloseZpHandles(var aHandles: TZpRWHandles); overload;
begin
  SafeCloseHandle(aHandles.ReadHandle);
  SafeCloseHandle(aHandles.WriteHandle);
end;

procedure CloseZpHandles(var aHandles: TZpRWEHandles); overload;
begin
  SafeCloseHandle(aHandles.ReadHandle);
  SafeCloseHandle(aHandles.WriteHandle);
  SafeCloseHandle(aHandles.ErrorHandle);
end;

//------------------------------------------------------------------------------

procedure CloseProcessHandles(var aProcessInfo: TProcessInformation);
begin
  if not SafeCloseHandle(aProcessInfo.hProcess) then
    RaiseLastOSError;
  if not SafeCloseHandle(aProcessInfo.hThread) then
    RaiseLastOSError;
end;

//------------------------------------------------------------------------------

procedure ConstructPipes(var aConsoleHandles: TZpRWEHandles;
  var aLocalHandles: TZpRWHandles);
var
  rSecurityDescriptor   : TSecurityDescriptor;
  rSecurityAttributes   : TSecurityAttributes;
  bCreateIsOK           : Boolean;

  procedure MakeHandleUninheritable(var aHandle: THandle);
  var
    hTempHandle         : THandle;
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
    begin
      if not SetHandleInformation(aHandle, HANDLE_FLAG_INHERIT, 0) then
        RaiseLastOSError
    end
    else
    begin
      // SetHandleInformation works only under NT, that's why we need to create
      // copy of the handle and destroy the original
      if not DuplicateHandle(GetCurrentProcess,
          aHandle,                      // Handle for copy
          GetCurrentProcess,
          @hTempHandle,                 // New handle
          0, False,                     // Do not inherit characteristics.
          DUPLICATE_SAME_ACCESS) then
        RaiseLastOSError;

      SafeCloseHandle(aHandle);
      aHandle := hTempHandle;
    end;
  end;

begin
  // Initialization of the arguments
  FillChar(aConsoleHandles, SizeOf(TZpRWHandles), 0);
  FillChar(aLocalHandles, SizeOf(TZpRWHandles), 0);
  // Initialization of working variables
  FillChar(rSecurityAttributes, SizeOf(TSecurityAttributes), 0);

  // Initialization of SecurityAttributes (and SecurityDescriptor, if under NT)
  rSecurityAttributes.nLength := SizeOf(TSecurityAttributes);
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    InitializeSecurityDescriptor(@rSecurityDescriptor, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(@rSecurityDescriptor, True, nil, False);
    rSecurityAttributes.lpSecurityDescriptor := @rSecurityDescriptor;
  end
  else
    rSecurityAttributes.lpSecurityDescriptor := nil;
  rSecurityAttributes.bInheritHandle := True;

  // Construction of the pipes.
  bCreateIsOK := False;
  try
    // Create pipe for output of the console application (to read from it).
    if not CreatePipe(aLocalHandles.ReadHandle, aConsoleHandles.WriteHandle,
        @rSecurityAttributes, 0) then
      RaiseLastOSError;

    // Create duplicate of the output stream from the console application to be
    // used also for standard stream for errors. This is necessary in case the
    // child application closes prematurely some of these standard output
    // streams (StdOut, StdErr).
    if not DuplicateHandle(GetCurrentProcess,
        aConsoleHandles.WriteHandle,    // Stream for copy
        GetCurrentProcess,
        @aConsoleHandles.ErrorHandle,   // New Handle's address (handle)
        0, True,                        // To inherit characteristics.
        DUPLICATE_SAME_ACCESS) then
      RaiseLastOSError;

    // Construct pipe for input in the console application (to write in it).
    if not CreatePipe(aConsoleHandles.ReadHandle, aLocalHandles.WriteHandle,
        @rSecurityAttributes, 0) then
      RaiseLastOSError;

    // The outputs of the pipes from Windows application should not inherit
    // attributs of the invoking process. In other case it will not be possible
    // to close automaticly the handles of both pipes and the operations with
    // them will most likely 'hang'.
    MakeHandleUninheritable(aLocalHandles.ReadHandle);
    MakeHandleUninheritable(aLocalHandles.WriteHandle);

    // Construction success
    bCreateIsOK := True;
  finally
    if not bCreateIsOK then
    begin
      // Error; close all handles if possible
      SafeCloseHandle(aConsoleHandles.ReadHandle);
      SafeCloseHandle(aConsoleHandles.WriteHandle);
      SafeCloseHandle(aConsoleHandles.ErrorHandle);
      SafeCloseHandle(aLocalHandles.ReadHandle);
      SafeCloseHandle(aLocalHandles.WriteHandle);
    end;
  end;
end;

//------------------------------------------------------------------------------
{ Help function. Contructs parameters for execution of the child process.
}
procedure ConstructStartupInfo(const aConsoleHandles: TZpRWEHandles;
  var aStartupInfo: TStartupInfo);
begin
  FillChar(aStartupInfo, SizeOf(TStartupInfo), 0);
  with aStartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    wShowWindow := SW_HIDE;
    hStdOutput := aConsoleHandles.WriteHandle;
    hStdInput := aConsoleHandles.ReadHandle;
    hStdError := aConsoleHandles.ErrorHandle;
  end;
end;

//------------------------------------------------------------------------------

function ReadFromPipe(const aHandle: THandle; var aBuffer;
  const aBufferSize: DWORD): DWORD;
var
  dwBytesAvail          : DWORD;
begin
  Result := 0;
  if PeekNamedPipe(aHandle, nil, 0, nil, @dwBytesAvail, nil) and
      (dwBytesAvail > 0) then
  begin
    dwBytesAvail := Min(aBufferSize, dwBytesAvail);
    if not ReadFile(aHandle, aBuffer, dwBytesAvail, Result, nil) then
    begin
      if GetLastError = ERROR_BROKEN_PIPE then
        Result := MAXDWORD
      else
        Result := 0;
    end;
  end
  else
    if GetLastError = ERROR_BROKEN_PIPE then
      Result := MAXDWORD;
end;

//------------------------------------------------------------------------------

function GetLineFromBuffer(var aBuffer: TZpBuffer; var aBufferPos: Integer;
  const aBufferLen: Integer; var aCurrentString: string; var aLine: string;
  const aFlushBuffer: Boolean = False): Boolean;
begin
  Result := True;
  aLine := EmptyStr;

  while (aBufferPos < aBufferLen) do
  begin
    if (aBuffer[aBufferPos] = #13) then
    begin
      if (aBufferPos < aBufferLen) and (aBuffer[aBufferPos+1] = #10) then
        Inc(aBufferPos);

      aLine := aCurrentString;
      aCurrentString := EmptyStr;
      Exit;
    end
    else
      if (aBuffer[aBufferPos] <> #10) then
        aCurrentString := aCurrentString + aBuffer[aBufferPos];

    Inc(aBufferPos);
  end;

  if aFlushBuffer then
  begin
    aLine := aCurrentString;
    aCurrentString := EmptyStr;
  end
  else
    Result := False;
end;

//------------------------------------------------------------------------------

function StopProcess(const aProcessInfo: TProcessInformation): Boolean;
var
  hProcessHandle        : THandle;
begin
  Result := True;
  if (aProcessInfo.hProcess = 0) then
    Exit;               // There is no started process, successful exit
  hProcessHandle := OpenProcess(PROCESS_TERMINATE, False,
    aProcessInfo.dwProcessId);
  if (hProcessHandle = 0) then
    RaiseLastOSError;  // Unsuccessful try to get handle
  Result := TerminateProcess(hProcessHandle, 0);
  CloseHandle(hProcessHandle);
end;

//------------------------------------------------------------------------------

function WriteToHandle(const aWriteHandle: THandle;
  const aText: string): Boolean;
var
  dwBytesWritten        : DWORD;
  dwTextLength          : Cardinal;
begin
  Result := False;
  if (aWriteHandle = 0) then
    Exit;               // There is no valid handle to write to; exit

  dwTextLength := Length(aText);
  if (dwTextLength > 0) then
    Result := WriteFile(aWriteHandle, aText[1], dwTextLength, dwBytesWritten,
      nil) and (dwBytesWritten = dwTextLength)
  else
    Result := True;
end;

//==============================================================================
// Interface procedures and functions (group for direct execution)
//==============================================================================

function ExecuteConsoleApp(const aCommandLine: string;
  const aCurrentDir: string = '';
  const aAppOutput: TStrings = nil;
  const aOnNewText: TZpOnNewTextEvent = nil;
  const aOnNewLine: TZpOnNewTextEvent = nil;
  const aEnvironment: TStrings = nil;
  const aProcessPriority: TZpProcessPriority = ppNormal): Cardinal;
var
  rConsoleHandles       : TZpRWEHandles;        // Will be used from the child process
  rLocalHandles         : TZpRWHandles;         // Will be used from the procedure
  rStartupInfo          : TStartupInfo;         // Params to start the process
  szEnvironment         : PChar;                // Help variable - environment of the process
  szCurrentDir          : PChar;                // Help variable - working dir of the process
  bAppTerminated        : Boolean;              // If the process is finished
  arBuffer              : TZpBuffer;            // Read buffer
  dwBytesRead           : DWORD;                // number of read bytes (of the process)
  sCurrentText          : string;               // Line with the received information so far from the process
  sNewLine              : string;               // Received full line from the process
  iBufferPos            : Integer;              // Position in the current buffer
begin
  Assert(aCommandLine <> EmptyStr);  // Is there command line for execution?

  szCurrentDir := PCharOrNil(Trim(aCurrentDir));

  // Pipe contruction
  ConstructPipes(rConsoleHandles, rLocalHandles);
  try
    ConstructStartupInfo(rConsoleHandles, rStartupInfo);

    szEnvironment := StringsToMultiSz(aEnvironment);
    try
      if not CreateProcess(nil, PChar(aCommandLine), nil, nil, True,
        CProcessPriorities[aProcessPriority] or CREATE_NO_WINDOW or
        CREATE_NEW_PROCESS_GROUP or CREATE_NEW_CONSOLE,
        szEnvironment, szCurrentDir, rStartupInfo, grProcessInfo) then
      begin
        // Error; let's try to close constructed handles
        CloseZpHandles(rLocalHandles);
        CloseProcessHandles(grProcessInfo);
        RaiseLastOSError;
      end;
    finally
      if (szEnvironment <> nil) then
        FreeMem(szEnvironment);
    end;

  finally
    // Destroy the handles of the pipes from the child process side (it already
    // has their copies). If we do not do this, the pipes will be active even
    // after the child process closes it's own copy of handles, which will
    // cause blocking of the read (information from the process).
    CloseZpHandles(rConsoleHandles);
  end;

  try
    // Remember the handle for write in a global variable
    ghWriteHandle := rLocalHandles.WriteHandle;

    // Let's start read from the input stream, untill the application stops
    sCurrentText := EmptyStr;
    repeat
      dwBytesRead := ReadFromPipe(rLocalHandles.ReadHandle, arBuffer, CZpBufferSize);

      bAppTerminated := (dwBytesRead = MAXDWORD);
      if not bAppTerminated and (dwBytesRead > 0) then
      begin
        arBuffer[dwBytesRead] := #0;
        if Assigned(aOnNewText) then
          aOnNewText(nil, arBuffer);

        if Assigned(aAppOutput) or Assigned(aOnNewLine) then
        begin
          iBufferPos := 0;
          while GetLineFromBuffer(arBuffer, iBufferPos, dwBytesRead,
              sCurrentText, sNewLine) do
          begin
            if Assigned(aAppOutput) then
              aAppOutput.Add(sNewLine);
            if Assigned(aOnNewLine) then
              aOnNewLine(nil, sNewLine);
          end;
        end;
      end;

      Application.ProcessMessages;
    until bAppTerminated;

    WaitForSingleObject(grProcessInfo.hProcess, CTerminationWaitTime);
    GetExitCodeProcess(grProcessInfo.hProcess, Result);

    // Clear the input bufffer
    iBufferPos := 0;
    if GetLineFromBuffer(arBuffer, iBufferPos, dwBytesRead, sCurrentText,
        sNewLine, True) then
    begin
      if Assigned(aAppOutput) then
        aAppOutput.Add(sNewLine);
      if Assigned(aOnNewLine) then
        aOnNewLine(nil, sNewLine);
    end;

    if (Result = STILL_ACTIVE) then
      Result := MAXDWORD;
  finally
    CloseProcessHandles(grProcessInfo);
  end;
end;

//------------------------------------------------------------------------------

function WriteToConsoleApp(const aText: string): Boolean;
begin
  Result := WriteToHandle(ghWriteHandle, aText);
end;

//------------------------------------------------------------------------------

function WriteLnToConsoleApp(const aText: string): Boolean;
begin
  Result := WriteToConsoleApp(aText + sLineBreak);
end;

//------------------------------------------------------------------------------

function StopConsoleApp: Boolean;
begin
  Result := StopProcess(grProcessInfo);
end;

//==============================================================================
// Thread for execution of console application
//==============================================================================
{ TZpExecConsoleAppThread }

constructor TZpExecConsoleAppThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FIsRunning := False;
  FCommandLine := EmptyStr;
  FProcessPriority := ppNormal;
  FAppOutput := nil;
  FEnvironment := TStringList.Create;
  FExitCode := 0;
  FLastError := 0;

  FOnNewText := nil;
  FOnNewLine := nil;
  FOnStatusChange := nil;
end;

//------------------------------------------------------------------------------

destructor TZpExecConsoleAppThread.Destroy;
begin
  inherited Destroy;
  FEnvironment.Free;
  FAppOutput.Free;
end;

//==============================================================================
{ private methods }

procedure TZpExecConsoleAppThread.SyncSendNewText;
begin
  FOnNewText(Self, FTextBuffer);
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.SyncSendNewLine;
begin
  FOnNewLine(Self, FTextBuffer);
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.SyncStatusChange;
begin
  FOnStatusChange(Self, FIsRunning);
end;

//==============================================================================
{ property methods }

procedure TZpExecConsoleAppThread.SetProcessPriority(
  const Value: TZpProcessPriority);
begin
  if not IsRunning and (Value <> FProcessPriority) then
    FProcessPriority := Value;
end;

//------------------------------------------------------------------------------

function TZpExecConsoleAppThread.GetUseAppOutput: Boolean;
begin
  Result := (FAppOutput <> nil);
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.SetUseAppOutput(const Value: Boolean);
begin
  if not IsRunning and (Value <> GetUseAppOutput) then
  begin
    if Value then
      FAppOutput := TStringList.Create
    else
      FAppOutput.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.SetEnvironment(const Value: TStringList);
begin
  if not IsRunning then
    FEnvironment.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.SetOnNewText(
  const Value: TZpOnNewTextEvent);
begin
  if not IsRunning then
    FOnNewText := Value;
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.SetOnNewLine(
  const Value: TZpOnNewTextEvent);
begin
  if not IsRunning then
    FOnNewLine := Value;
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.SetOnStatusChange(
  const Value: TZpOnStatusChange);
begin
  if not IsRunning then
    FOnStatusChange := Value;
end;

//==============================================================================
{ protected methods }

procedure TZpExecConsoleAppThread.DoChangeStatus(
  const aIsRunning: Boolean);
begin
  if (FIsRunning <> aIsRunning) then
  begin
    FIsRunning := aIsRunning;
    if Assigned(FOnStatusChange) then
      Synchronize(SyncStatusChange);
  end;
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.DoSendNewText(const aNewText: string);
begin
  if (aNewText <> EmptyStr) and Assigned(FOnNewText) then
  begin
    FTextBuffer := aNewText;
    Synchronize(SyncSendNewText);
  end;
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.DoSendNewLine(const aNewLine: string);
begin
  if Assigned(FAppOutput) then
    FAppOutput.Add(aNewLine);
  if Assigned(FOnNewLine) then
  begin
    FTextBuffer := aNewLine;
    Synchronize(SyncSendNewLine);
  end;
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.ChangeStatus(const aIsRunning: Boolean);
begin
  DoChangeStatus(aIsRunning);
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.SendNewText(const aNewText: string);
begin
  DoSendNewText(aNewText);
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.SendNewLine(const aNewLine: string);
begin
  DoSendNewLine(aNewLine);
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.DoExecConsole;
var
  rConsoleHandles       : TZpRWEHandles;        // Will be used by the child process
  rLocalHandles         : TZpRWHandles;         // Will be used by the procedure
  szCurrentDir          : PChar;                // Help variable - working dir of the process
  szEnvironment         : PChar;                // Help variable - environment of the process
  bAppTerminated        : Boolean;              // Is the process finished
  arBuffer              : TZpBuffer;            // Read buffer
  dwBytesRead           : DWORD;                // Number of read bytes(by the process)
  sCurrentText          : string;               // Line with the recevied so far information
  sNewLine              : string;               // Received line from the process
  iBufferPos            : Integer;              // Position in the current buffer
begin
  try
    szCurrentDir := PCharOrNil(Trim(FCurrentDir));

    // Pipe contruction
    ConstructPipes(rConsoleHandles, rLocalHandles);
    try
      ConstructStartupInfo(rConsoleHandles, FStartupInfo);

      szEnvironment := StringsToMultiSz(FEnvironment);
      try
        if not CreateProcess(nil, PChar(FCommandLine), nil, nil, True,
          CProcessPriorities[FProcessPriority] or CREATE_NO_WINDOW or
          CREATE_NEW_PROCESS_GROUP or CREATE_NEW_CONSOLE,
          szEnvironment, szCurrentDir, FStartupInfo, FProcessInfo) then
        begin
          // Error; let's try to close the handles
          CloseZpHandles(rLocalHandles);
          CloseProcessHandles(FProcessInfo);
          RaiseLastOSError;
        end;
      finally
        if (szEnvironment <> nil) then
          FreeMem(szEnvironment);
      end;

    finally
      // Destroy the handles of the pipes from the child process side (it
      // already has their copies). If we do not do this, the pipes will be
      // active even after the child process closes it's own copy of handles,
      // which will cause blocking of the read (information from the process).
      CloseZpHandles(rConsoleHandles);
    end;

    try
      // Remember the handle for write
      FWriteHandle := rLocalHandles.WriteHandle;

      // Start reading the input stream, untill the application stops
      sCurrentText := EmptyStr;
      repeat
        bAppTerminated := False;
        if not ReadFile(rLocalHandles.ReadHandle, arBuffer, CZpBufferSize,
            dwBytesRead, nil) then
        begin
          if GetLastError = ERROR_BROKEN_PIPE then
            bAppTerminated := True
          else
            dwBytesRead := 0;
        end;

        if not bAppTerminated and (dwBytesRead > 0) then
        begin
          arBuffer[dwBytesRead] := #0;
          SendNewText(arBuffer);

          iBufferPos := 0;
          while GetLineFromBuffer(arBuffer, iBufferPos, dwBytesRead,
              sCurrentText, sNewLine) do
          begin
            SendNewLine(sNewLine);
          end;
        end;
      until bAppTerminated;

      WaitForSingleObject(FProcessInfo.hProcess, CTerminationWaitTime);
      GetExitCodeProcess(FProcessInfo.hProcess, FExitCode);

      // Clear the input buffer
      iBufferPos := 0;
      if GetLineFromBuffer(arBuffer, iBufferPos, dwBytesRead, sCurrentText,
          sNewLine, True) then
      begin
        SendNewLine(sNewLine);
      end;

      if (FExitCode = STILL_ACTIVE) then
        FExitCode := MAXDWORD;
    finally
      CloseProcessHandles(FProcessInfo);
    end;
  except
    on E: EOSError do
      FLastError := E.ErrorCode;
  end;

  FWriteHandle := 0;
end;

//------------------------------------------------------------------------------

procedure TZpExecConsoleAppThread.Execute;
begin
  while not Terminated do
  begin
    ChangeStatus(True);
    try
      DoExecConsole;
    finally
      ChangeStatus(False);
    end;
    Suspend;
  end;
end;

//==============================================================================
{ public methods }

function TZpExecConsoleAppThread.Run(const aCommandLine: string;
  const aCurrentDir: string = ''): Boolean;
begin
  Result := False;
  if IsRunning or (aCommandLine = EmptyStr) then
    Exit;

  // Set params
  FCommandLine := aCommandLine;
  FCurrentDir := aCurrentDir;
  if Assigned(FAppOutput) then
    FAppOutput.Clear;
  FExitCode := MAXDWORD;
  FLastError := 0;
  FWriteHandle := 0;

  // Execute
  Resume;
  Result := True;
end;

//------------------------------------------------------------------------------

function TZpExecConsoleAppThread.Write(const aText: string): Boolean;
begin
  Result := IsRunning and WriteToHandle(FWriteHandle, aText);
end;

//------------------------------------------------------------------------------

function TZpExecConsoleAppThread.WriteLn(const aLine: string): Boolean;
begin
  Result := IsRunning and WriteToHandle(FWriteHandle, aLine + sLineBreak);
end;

//------------------------------------------------------------------------------

function TZpExecConsoleAppThread.Stop: Boolean;
begin
  Result := IsRunning and StopProcess(FProcessInfo);
end;

//==============================================================================

end.

