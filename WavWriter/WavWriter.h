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

class CWavWriterInputPin;
class CWavWriterFilter;

//------------------------------------------------------------------------------

typedef struct
{
	short Max;
	short Min;
} Peak;

const char* PeakFileID = "PeakFile";
const unsigned int PeakFileVer = 0x0100;

#pragma pack(push, 1)

typedef struct {
	char ID[8];
	unsigned int Version;
	int LengthMs;
	DWORD SamplePerSec;
	WORD Channels;
	WORD BitsPerSample;
	DWORD SamplePerPeak;
	DWORD PeakTabLen;
} PeakFileHeader;

#pragma pack(pop)

//------------------------------------------------------------------------------

class CWavWriterFilter : public CBaseFilter
                         , IFileSinkFilter
						 , IMediaSeeking
						 , IWavWriter
{
	friend class CWavWriterInputPin;
	
    CWavWriterInputPin *m_pPin;          // A simple rendered input pin
	CCritSec m_ReceiveLock;              // Lock for received samples
	CCritSec m_Lock;					 // Main renderer critical section

	bool m_Writting;
	bool m_bWriteWavFile;
	CWAVFileWriter* m_WavFile;
    LPOLESTR m_pFileName;           // The filename where we write
	
	REFERENCE_TIME m_rtDuration;
	DWORD m_dwSeekingCaps;

	ULONG m_cbWavData;
	WAVEFORMATEX* m_OutputWF;
	CMediaType m_OutType;
	int m_InputSampleRate;
	int m_InputChannels;
	int m_InputBitsPerSample;
    int m_IsPCM;

	bool m_bFastConvertMode;
	DWORD m_SamplePerPeakRatio;
	DWORD m_SamplePerPeak;
	bool m_bWritePeakFile;
	LPOLESTR m_pPeakFileName;

	int  m_OutBuffSize;
	char* m_OutBuff;
	int m_ConvBuffSize;
	char* m_ConvBuff;	
	CircBuffer* m_ConvCircBuffer;


	Peak m_Peak;
	PeakFileHeader m_PeakFileHeader;
	CWin32File* m_PeakFile;
	CircBuffer* m_PeakCircBuffer;
	char* m_PeakCalcBuffer;
	int m_PeakCalcBufferSize;	
	int m_InputAllocatorBuffSize;

    ULONG dataTotalOut;
	

public:

    DECLARE_IUNKNOWN

	// Constructor
	CWavWriterFilter(LPUNKNOWN pUnk, HRESULT *phr);
	virtual ~CWavWriterFilter();

	static CUnknown * WINAPI CreateInstance(LPUNKNOWN punk, HRESULT *phr);
	STDMETHODIMP NonDelegatingQueryInterface(REFIID riid, void **ppv);

	HRESULT Write(PBYTE pbData, LONG lDataLength);
	HRESULT Convert(char* pInData, long lInLength, char* pOutData, long *lOutLength, bool finalize);
	HRESULT StartWriting();
	HRESULT StopWriting();
	HRESULT InitPeakData();
	void PeakProcessing(BYTE* pInData, LONG lInLength, bool finalize);
	void CleanPeakData();

    // Pin enumeration
    CBasePin * GetPin(int n);
    int GetPinCount();

    // Open and close the file as necessary
    STDMETHODIMP Run(REFERENCE_TIME tStart);
    STDMETHODIMP Pause();
    STDMETHODIMP Stop();
	
    // Implements the IFileSinkFilter interface
    STDMETHODIMP SetFileName(LPCOLESTR pszFileName,const AM_MEDIA_TYPE *pmt);
    STDMETHODIMP GetCurFile(LPOLESTR * ppszFileName,AM_MEDIA_TYPE *pmt);

	// IMediaSeeking
	STDMETHODIMP GetCapabilities(DWORD* pCapabilities);
	STDMETHODIMP CheckCapabilities(DWORD* pCapabilities);
	STDMETHODIMP IsFormatSupported(const GUID* pFormat);
	STDMETHODIMP QueryPreferredFormat(GUID* pFormat);
	STDMETHODIMP GetTimeFormat(GUID* pFormat);
	STDMETHODIMP IsUsingTimeFormat(const GUID* pFormat);
	STDMETHODIMP SetTimeFormat(const GUID* pFormat);
	STDMETHODIMP GetDuration(LONGLONG* pDuration);
	STDMETHODIMP GetStopPosition(LONGLONG* pStop);
	STDMETHODIMP GetCurrentPosition(LONGLONG* pCurrent);
	STDMETHODIMP ConvertTimeFormat(LONGLONG* pTarget, const GUID* pTargetFormat, LONGLONG Source, const GUID* pSourceFormat);
	STDMETHODIMP SetPositions(LONGLONG* pCurrent, DWORD dwCurrentFlags, LONGLONG* pStop, DWORD dwStopFlags);
	STDMETHODIMP GetPositions(LONGLONG* pCurrent, LONGLONG* pStop);
	STDMETHODIMP GetAvailable(LONGLONG* pEarliest, LONGLONG* pLatest);
	STDMETHODIMP SetRate(double dRate);
	STDMETHODIMP GetRate(double* pdRate);
	STDMETHODIMP GetPreroll(LONGLONG* pllPreroll);

	// IWavWriter
	STDMETHODIMP SetFastConversionMode(DWORD FastConvertMode);
	STDMETHODIMP GetFastConversionMode(DWORD *FastConvertMode);
	STDMETHODIMP SetSamplesPerPeakRatio(DWORD SamplePerPeakRatio);
	STDMETHODIMP SetWritePeakFile(DWORD WritePeakFile);
	STDMETHODIMP SetPeakFileName(LPCOLESTR pszFileName);	
	STDMETHODIMP SetWriteWavFile(DWORD WriteWavFile);
};


//  Pin object

class CWavWriterInputPin : public CRenderedInputPin
{
    CWavWriterFilter * const m_pFilter;
    CCritSec * const m_pReceiveLock;    // Sample critical section    
	REFERENCE_TIME m_tPrevStart;
	REFERENCE_TIME m_tPrevStop;

public:

    CWavWriterInputPin(CWavWriterFilter *pFilter,
				  CCritSec *pLock,
                  CCritSec *pReceiveLock,
                  HRESULT *phr);

    // Do something with this media sample
    STDMETHODIMP Receive(IMediaSample *pSample);
    STDMETHODIMP EndOfStream(void);
    STDMETHODIMP ReceiveCanBlock();
	STDMETHODIMP NewSegment(REFERENCE_TIME tStart, REFERENCE_TIME tStop, double dRate);

    // Check if the pin can support this specific proposed type and format
    HRESULT CheckMediaType(const CMediaType *);

	HRESULT CompleteConnect(IPin *pReceivePin);
};
