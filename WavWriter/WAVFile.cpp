// -----------------------------------------------------------------------------
//  VisualSubSync
// -----------------------------------------------------------------------------
//  Copyright (C) 2003 Christophe Paris
// -----------------------------------------------------------------------------
//  This Program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2, or (at your option)
//  any later version.
//
//  This Program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with GNU Make; see the file COPYING.  If not, write to
//  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
//  http://www.gnu.org/copyleft/gpl.html
// -----------------------------------------------------------------------------

#include <windows.h>
#include "WAVFile.h"

// ============================================================================

int CWin32File::Open(TCHAR* Filename, CWin32FileOpenMode OpenMode)
{
	if(OpenMode == CWin32File_READ_MODE)
	{
		m_hFile = CreateFile(Filename, GENERIC_READ, FILE_SHARE_READ, NULL,
			OPEN_EXISTING, (DWORD) 0, NULL);
	} else {
		m_hFile = CreateFile(Filename, GENERIC_WRITE, FILE_SHARE_READ, NULL,
			CREATE_ALWAYS, (DWORD) 0, NULL);
	}
	
	if(m_hFile == INVALID_HANDLE_VALUE)
	{
		return 0;
	}
	
	// Get file size
	DWORD dwSizeLow, dwSizeHigh, dwError;
	dwSizeLow = GetFileSize(m_hFile, &dwSizeHigh);
	if (dwSizeLow == 0xFFFFFFFF && (dwError = GetLastError()) != NO_ERROR )
	{ 
		CloseHandle(m_hFile);
		m_hFile = INVALID_HANDLE_VALUE;
		return 0;
	}
	m_i64FileSize = (dwSizeHigh << 32) | dwSizeLow;	
	return 1;
}

// ----------------------------------------------------------------------------

int CWin32File::Open2(LPOLESTR Filename, CWin32FileOpenMode OpenMode)
{

    TCHAR *pFileName = NULL;
    
#if defined(WIN32) && !defined(UNICODE)
    char convert[MAX_PATH];
    
    if(!WideCharToMultiByte(CP_ACP, 0, Filename, -1, convert, MAX_PATH, 0, 0))
        return 0;
    
    pFileName = convert;
#else
    pFileName = Filename;
#endif

	return Open(pFileName, OpenMode);
}

// ----------------------------------------------------------------------------

unsigned int CWin32File::Read(void *Buffer, unsigned int Size)
{
	DWORD dwNumberOfBytesRead = 0;
	if(!ReadFile(m_hFile, Buffer, Size, &dwNumberOfBytesRead, NULL))
	{
		return 0;
	}
	return dwNumberOfBytesRead;
}

// ----------------------------------------------------------------------------

unsigned int CWin32File::Write(void *Buffer, unsigned int Size)
{
	DWORD dwNumberOfBytesWritten = 0;		
	if (!WriteFile(m_hFile, Buffer, Size, &dwNumberOfBytesWritten, NULL)) 
	{
		return 0;
	}	
	return dwNumberOfBytesWritten;
}
// ----------------------------------------------------------------------------

__int64 CWin32File::Seek(__int64 Offset, int Origin)
{
	DWORD dwPointerLow, dwError;
	LONG lOffLow  = (LONG)(Offset & 0x00000000FFFFFFFF);
	LONG lOffHigh = (LONG)((Offset >> 32) & 0x00000000FFFFFFFF);
	
	dwPointerLow = SetFilePointer(m_hFile, lOffLow, &lOffHigh, Origin);
	
	if (dwPointerLow == 0xFFFFFFFF && (dwError = GetLastError()) != NO_ERROR )
	{
		return -1;		
	}
	
	return ((lOffHigh << 32) & 0xFFFFFFFF00000000) | dwPointerLow;
}

// ----------------------------------------------------------------------------

__int64 CWin32File::Position()
{
	LONG lPosLow = 0, lPosHigh = 0;
	lPosLow = SetFilePointer(m_hFile, 0, &lPosHigh, FILE_CURRENT);
	return ((lPosHigh << 32) & 0xFFFFFFFF00000000) | lPosLow;
}

// ----------------------------------------------------------------------------

void CWin32File::Close()
{
	if(m_hFile != INVALID_HANDLE_VALUE)
	{		
		CloseHandle(m_hFile);
        m_hFile = INVALID_HANDLE_VALUE;
	}
}

// ============================================================================