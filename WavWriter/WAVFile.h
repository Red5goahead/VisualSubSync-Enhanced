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

// ----------------------------------------------------------------------------
// Very basic WAV file class handling
// ----------------------------------------------------------------------------

typedef enum {
	CWin32File_READ_MODE,
	CWin32File_WRITE_MODE
} CWin32FileOpenMode;

class CWin32File
{
public:
	CWin32File() : m_hFile(INVALID_HANDLE_VALUE), m_i64FileSize(0) { }
	~CWin32File() { Close(); }
	int Open(TCHAR* Filename, CWin32FileOpenMode OpenMode);
	int Open2(LPOLESTR Filename, CWin32FileOpenMode OpenMode);
	unsigned int Read(void *Buffer, unsigned int Size);
	unsigned int Write(void *Buffer, unsigned int Size);
	bool CanSeek() { return true; }	
	__int64 Seek(__int64 Offset, int Origin);
	__int64 Position();
	__int64 Size() { return m_i64FileSize; }	
	void Close();
	
private:
	HANDLE m_hFile;
	__int64 m_i64FileSize;
};

// ----------------------------------------------------------------------------

class CWAVFileWriter
{
public:
	CWAVFileWriter() : m_File(NULL) { }

	virtual ~CWAVFileWriter()
	{
		Close();
	}
	
	bool Init(LPOLESTR Filename, WAVEFORMATEX* wf)
	{
		m_File = new CWin32File();
		if(!m_File->Open2(Filename,CWin32File_WRITE_MODE))
		{
			return false;
		}

		m_DataHdrOffset = 0;
		m_PCMDataSize = 0;
		
		return WriteRIFFHeader(0) && // Dummy value
			WriteFORMATHeader(wf) &&
			WriteDATAHeader(0); // Dummy value
	}

	bool WritePCMData(void* Data, unsigned int Size)
	{
		unsigned int DataWritten = m_File->Write(Data,Size);
		m_PCMDataSize += DataWritten;
		return (DataWritten == Size);
	}

private:
	bool WriteRIFFHeader(unsigned int Size)
	{	
		return (m_File->Write("RIFF", 4) == 4) &&
			(m_File->Write(&Size, 4) == 4) && // This is file size - 8.
			(m_File->Write("WAVE", 4) == 4);
	}

	bool WriteFORMATHeader(WAVEFORMATEX* wf)
	{
		unsigned int Size = sizeof(WAVEFORMATEX);
		return (m_File->Write("fmt ", 4) == 4) &&
			(m_File->Write(&Size, 4) == 4) &&
			(m_File->Write(wf, Size) == Size);
	}

	HRESULT WriteDATAHeader(unsigned int Size)
	{
		m_DataHdrOffset = (unsigned int)m_File->Position();
		return (m_File->Write("data", 4) == 4) &&
			(m_File->Write(&Size, 4) == 4); // PCM data size
	}

	void PatchHeaders()
	{
		// Get file size (we are already at the end :))
		unsigned int Filesize = (unsigned int)m_File->Position();
		m_File->Seek(0,FILE_BEGIN);

		// Rewrite RIFF header
		WriteRIFFHeader(Filesize-8);
		
		// Write data size now
		m_File->Seek(m_DataHdrOffset,FILE_BEGIN);
		WriteDATAHeader(m_PCMDataSize);
	}

	void Close()
	{
		if (m_File) {
			PatchHeaders();
			delete m_File;
			m_File = NULL;
		}
	}

	CWin32File* m_File;
	unsigned int m_DataHdrOffset;
	unsigned int m_PCMDataSize;
};

// ----------------------------------------------------------------------------