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
#include <commdlg.h>
#include <streams.h>
#include <initguid.h>
#include <mmreg.h>

#include <stdio.h>

#include "CircBuff.h"
#include "WAVFile.h"
#include "IWavWriter.h"
#include "WavWriterUIDs.h"
#include "WavWriter.h"


// Setup data

const AMOVIESETUP_MEDIATYPE sudPinTypes =
{
    &MEDIATYPE_Audio,          // Major type
    &MEDIASUBTYPE_PCM          // Minor type
};

const AMOVIESETUP_PIN sudPins =
{
    L"Input",                   // Pin string name
    FALSE,                      // Is it rendered
    FALSE,                      // Is it an output
    FALSE,                      // Allowed none
    FALSE,                      // Likewise many
    &CLSID_NULL,                // Connects to filter
    L"Output",                  // Connects to pin
    1,                          // Number of types
    &sudPinTypes                // Pin information
};

const AMOVIESETUP_FILTER sudWavWriter =
{
    &CLSID_WavWriter,           // Filter CLSID
    L"WAV Writer",               // String name
    MERIT_DO_NOT_USE,           // Filter merit
    1,                          // Number pins
    &sudPins                    // Pin details
};


//
//  Object creation stuff
//
CFactoryTemplate g_Templates[]= {
    L"WAV Writer", &CLSID_WavWriter, CWavWriterFilter::CreateInstance, NULL, &sudWavWriter
};
int g_cTemplates = 1;

#ifdef _DEBUG
void DebugLog(TCHAR *pFormat,...)
{
    TCHAR szMsg[2000];
    va_list va;     
    va_start(va, pFormat);
    wvsprintf(szMsg, pFormat, va);
    lstrcat(szMsg, TEXT("\r\n"));
    OutputDebugString(szMsg);
    va_end(va);
}
#else
#define DebugLog
#endif

//-----------------------------------------------------------------------------

CUnknown * WINAPI CWavWriterFilter::CreateInstance(LPUNKNOWN punk, HRESULT *phr)
{
    ASSERT(phr);
    
    CWavWriterFilter *pNewObject = new CWavWriterFilter(punk, phr);
    if (pNewObject == NULL) {
        if (phr)
            *phr = E_OUTOFMEMORY;
    }
    
    return pNewObject;  
}

//-----------------------------------------------------------------------------

CWavWriterFilter::CWavWriterFilter(LPUNKNOWN pUnk, HRESULT *phr) :
    CBaseFilter(NAME("CWavWriterFilter"), pUnk, &m_Lock, CLSID_WavWriter),
    m_pPin(NULL),
    m_pFileName(0),
    m_Writting(false),
    m_dwSeekingCaps(AM_SEEKING_CanGetDuration | AM_SEEKING_CanGetCurrentPos),
    m_rtDuration(0),
    m_cbWavData(0),
    m_WavFile(NULL),
    m_bFastConvertMode(false),
    m_OutBuffSize(0),
    m_OutBuff(NULL),
    m_bWritePeakFile(false),
    m_pPeakFileName(0),
    m_PeakFile(NULL),
    m_PeakCircBuffer(NULL),
    m_PeakCalcBuffer(NULL),
    m_InputAllocatorBuffSize(0),
    m_bWriteWavFile(true),
    m_ConvCircBuffer(NULL),
    m_ConvBuffSize(0),
    m_ConvBuff(NULL),
	m_InputBitsPerSample(0),
	m_InputSampleRate(0),
	m_InputChannels(0),
    m_IsPCM(0)
{
    m_pPin = new CWavWriterInputPin(this, &m_Lock, &m_ReceiveLock, phr);
    if (m_pPin == NULL) {
        if (phr)
            *phr = E_OUTOFMEMORY;
        return;
    }
}

//-----------------------------------------------------------------------------

CWavWriterFilter::~CWavWriterFilter()
{
    delete m_pPin;
	m_pPin = NULL;
    delete m_pFileName;
	m_pFileName = NULL;
    delete m_pPeakFileName;
	m_pPeakFileName = NULL;
    delete[] m_OutBuff;
	m_OutBuff = NULL;
    delete[] m_ConvBuff;
	m_ConvBuff = NULL;
    delete m_ConvCircBuffer;
	m_ConvCircBuffer = NULL;
}

//-----------------------------------------------------------------------------

STDMETHODIMP CWavWriterFilter::NonDelegatingQueryInterface(REFIID riid, void ** ppv)
{
    CheckPointer(ppv,E_POINTER);
    CAutoLock lock(&m_Lock);
    
    // Do we have this interface
    if (riid == IID_IFileSinkFilter)
    {
        return GetInterface((IFileSinkFilter *) this, ppv);
    }
    else if (riid == IID_IMediaSeeking)
    {
        return GetInterface((IMediaSeeking *) this, ppv);
    }
    else if (riid == IID_IWavWriter)
    {
        return GetInterface((IWavWriter *)this, ppv);
    }
    
    return CBaseFilter::NonDelegatingQueryInterface(riid, ppv);
}

//-----------------------------------------------------------------------------

CBasePin * CWavWriterFilter::GetPin(int n)
{
    if (n == 0) {
        return m_pPin;
    } else {
        return NULL;
    }
}

//-----------------------------------------------------------------------------

int CWavWriterFilter::GetPinCount()
{
    return 1;
}

//-----------------------------------------------------------------------------

HRESULT CWavWriterFilter::StartWriting()
{
    if(!m_Writting)
    {
        // Get allocator size
        ALLOCATOR_PROPERTIES InProps;
        IMemAllocator * pInAlloc = NULL;        
        HRESULT hr;
        
        hr = m_pPin->GetAllocator(&pInAlloc);
        if(SUCCEEDED(hr))
        {
            hr = pInAlloc->GetProperties(&InProps);
            m_InputAllocatorBuffSize = InProps.cbBuffer;
            pInAlloc->Release();
            DebugLog(TEXT("CWavWriterFilter::StartWriting m_InputAllocatorBuffSize=%d"),
                m_InputAllocatorBuffSize);
        }       

        if(m_bWriteWavFile)
        {   
            m_WavFile = new CWAVFileWriter();
            if(!m_WavFile->Init(m_pFileName, m_OutputWF))
            {
                delete m_WavFile;
                m_WavFile = NULL;
                return S_FALSE;
            }

            m_ConvBuffSize = 512 * (m_InputChannels * (m_InputBitsPerSample / 8)) * 2 * 2;
            if(m_ConvBuff)
                delete[] m_ConvBuff;
            m_ConvBuff = new char[m_ConvBuffSize];
            DebugLog(TEXT("CWavWriterFilter::StartWriting m_ConvBuffSize=%d"), m_ConvBuffSize);
            
            m_OutBuffSize = max((m_InputAllocatorBuffSize * 2), m_ConvBuffSize);
            if(m_OutBuff)
                delete[] m_OutBuff;
            m_OutBuff = new char[m_OutBuffSize];
            DebugLog(TEXT("CWavWriterFilter::StartWriting m_OutBuffSize=%d"), m_OutBuffSize);
            
            if(m_bFastConvertMode)      
            {           
                if(m_ConvCircBuffer)
                    delete m_ConvCircBuffer;
                m_ConvCircBuffer = new CircBuffer(max(m_InputAllocatorBuffSize * 2, m_ConvBuffSize * 2));
            }
        }

        InitPeakData();
        m_Writting = true;
        dataTotalOut = 0;
    }
    return S_OK;
}

//-----------------------------------------------------------------------------

HRESULT CWavWriterFilter::StopWriting()
{
    if(m_Writting)
    {
        if(m_WavFile)
        {
            if(m_bFastConvertMode)
            {           
                LONG lOutLength = 0;
                Convert(NULL, 0, m_OutBuff, &lOutLength, true);
                PeakProcessing((PBYTE)m_OutBuff, lOutLength, true);
                dataTotalOut += lOutLength;
                m_WavFile->WritePCMData(m_OutBuff, lOutLength);
            }
            delete m_WavFile;
            m_WavFile = NULL;
        }
        CleanPeakData();
        m_Writting = false;
    }
    DebugLog(TEXT("CWavWriterFilter::StopWriting m_cbWavData=%d, dataTotalOut=%d"),
        m_cbWavData, dataTotalOut);
    m_cbWavData = 0;
    return S_OK;
}

//-----------------------------------------------------------------------------

STDMETHODIMP CWavWriterFilter::Stop()
{
    CAutoLock cObjectLock(m_pLock);
    StopWriting();
    return CBaseFilter::Stop();
}

//-----------------------------------------------------------------------------

STDMETHODIMP CWavWriterFilter::Pause()
{
    CAutoLock cObjectLock(m_pLock);
    StartWriting();
    return CBaseFilter::Pause();
}

//-----------------------------------------------------------------------------

STDMETHODIMP CWavWriterFilter::Run(REFERENCE_TIME tStart)
{
    CAutoLock cObjectLock(m_pLock);
    StartWriting();
    return CBaseFilter::Run(tStart);
}

//-----------------------------------------------------------------------------

HRESULT CWavWriterFilter::Write(PBYTE pbData, LONG lDataLength)
{
    CAutoLock cObjectLock(m_pLock);

    if(!m_Writting)
        return S_FALSE;

    m_cbWavData += lDataLength;

    if(!m_bFastConvertMode) {
        PeakProcessing(pbData, lDataLength, false);
    }

    if(m_WavFile)
    {
        if (lDataLength > m_OutBuffSize) {
            // Change buffer size
            m_OutBuffSize = (lDataLength * 2);
            if(m_OutBuff)
                delete[] m_OutBuff;
            m_OutBuff = new char[m_OutBuffSize];
        }

        LONG lOutLength = m_OutBuffSize;
        Convert((char*)pbData, lDataLength, m_OutBuff, &lOutLength, false);

        if(m_bFastConvertMode) {
            PeakProcessing((PBYTE)m_OutBuff, lOutLength, false);
        }
        dataTotalOut += lOutLength;
        if(!m_WavFile->WritePCMData(m_OutBuff, lOutLength))
        {
            return S_FALSE;
        }
    }
    return S_OK;
}

//-----------------------------------------------------------------------------

static short float2short(float value) {
    int valueI = (int)(value * 32767);
    if (valueI > 32767) {
        valueI = 32767; 
    } else if(valueI < -32768) {
        valueI = -32768;
    }
    return (short) valueI;
}

HRESULT CWavWriterFilter::Convert(char* pInData, long lInLength,
                                  char* pOutData, long *lOutLength,
                                  bool finalize)
{
    if(m_bFastConvertMode)
    {
        *lOutLength = 0;
        
        DebugLog(TEXT("CWavWriterFilter::Convert lInLength=%d"),lInLength);

        if (lInLength > m_ConvCircBuffer->Size()) {
            m_ConvCircBuffer->ChangeSize(lInLength * 2);
        }

        if(pInData)
        {
            m_ConvCircBuffer->Write(pInData, lInLength);
        }
        
        int BytesToRead = 0, BytesAvailable = 0;

        while( ((BytesAvailable = m_ConvCircBuffer->Filled()) >= m_ConvBuffSize) || finalize)
        {
            if(finalize && (BytesAvailable < m_ConvBuffSize))
            {
                ZeroMemory(m_ConvBuff, m_ConvBuffSize);
                BytesToRead = BytesAvailable;
                finalize = false;
            } else {
                BytesToRead = m_ConvBuffSize;
            }
            
            m_ConvCircBuffer->Read(m_ConvBuff, BytesToRead);

            switch(m_InputBitsPerSample)
            {
            case 8:
                {
                    char* src = (char*) m_ConvBuff;
                    char* dst = (char*) pOutData;
                    int sample;
                    int i = 0, j = 0;
                    int loop = BytesToRead / sizeof(char) / m_InputChannels / 2;
                    while (i < loop)
                    {
                        // Read and convert one sample to mono
                        sample = 0;
                        for (j = 0; j < m_InputChannels; ++j) {
                            sample += *src++;
                        }
                        sample /= m_InputChannels;
                        // Write the sample
                        *dst++ = (char) sample;
                        // Skip next sample
                        src += m_InputChannels;
                        i++;
                    }
                    *lOutLength += i * sizeof(char);
                    pOutData += i * sizeof(char);
                }
                break;
                
            case 16:
                {
                    short* src = (short*) m_ConvBuff;
                    short* dst = (short*) pOutData;
                    int sample;
                    int i = 0, j = 0;
                    int loop = BytesToRead / sizeof(short) / m_InputChannels / 2;
                    while (i < loop)
                    {
                        // Read and convert one sample to mono
                        sample = 0;
                        for (j = 0; j < m_InputChannels; ++j) {
                            sample += *src++;
                        }
                        sample /= m_InputChannels;
                        // Skip next sample
                        src += m_InputChannels;
                        // Write the sample
                        *dst++ = (short) sample;
                        i++;
                    }
                    *lOutLength += i * sizeof(short);
                    pOutData += i * sizeof(short);
                }
                break;

            case 32:
                if (m_IsPCM)
                {
                    int* src = (int*) m_ConvBuff;
                    short* dst = (short*) pOutData;
                    __int64 sample;
                    int i = 0, j = 0;
                    int loop = BytesToRead / sizeof(int) / m_InputChannels / 2;
                    while (i < loop)
                    {
                        // Read and convert one sample to mono
                        sample = 0;
                        for (j = 0; j < m_InputChannels; ++j) {
                            sample += *src++;
                        }
                        sample /= m_InputChannels;
                        // Skip next sample
                        src += m_InputChannels;
                        // Write the sample
                        *dst++ = (short) (sample >> 16);
                        i++;
                    }
                    *lOutLength += i * sizeof(short);
                    pOutData += i * sizeof(short);
                } else {
                    // Convert from 32 bits float to 16 bits integer
                    float* src = (float*)m_ConvBuff;
                    short* dst = (short*)pOutData;
                    float sample;
                    int i = 0, j = 0;
                    int loop = BytesToRead / sizeof(float) / m_InputChannels / 2;
                    while (i < loop)
                    {
                        // Read and convert one sample to mono
                        sample = 0;
                        for (j = 0; j < m_InputChannels; ++j) {
                            sample += *src++;
                        }
                        sample /= m_InputChannels;
                        // Skip next sample
                        src += m_InputChannels;
                        // Write the sample
                        *dst++ = float2short(sample);
                        i++;
                    }
                    *lOutLength += i * sizeof(short);
                    pOutData += i * sizeof(short);
                }
                break;
            }
        }
    } else {
        CopyMemory((PVOID) pOutData,(PVOID) pInData, lInLength);        
        *lOutLength = lInLength;
    }
    DebugLog(TEXT("CWavWriterFilter::Convert lOutLength=%d"),*lOutLength);
    return S_OK;
}


//-----------------------------------------------------------------------------

HRESULT CWavWriterFilter::InitPeakData()
{
    if(m_bWritePeakFile)
    {   
        m_SamplePerPeak = m_InputSampleRate / m_SamplePerPeakRatio;
        if(m_bFastConvertMode)
            m_SamplePerPeak /= 2;
        
        ZeroMemory(&m_PeakFileHeader, sizeof(m_PeakFileHeader));
        strncpy(m_PeakFileHeader.ID, PeakFileID,8);
        m_PeakFileHeader.SamplePerSec = m_OutputWF->nSamplesPerSec;
        m_PeakFileHeader.Channels = m_OutputWF->nChannels;
        m_PeakFileHeader.BitsPerSample = m_OutputWF->wBitsPerSample;
        m_PeakFileHeader.Version = PeakFileVer;
        m_PeakFileHeader.SamplePerPeak = m_SamplePerPeak;        
        m_PeakFile = new CWin32File();
        if(!m_PeakFile->Open2(m_pPeakFileName, CWin32File_WRITE_MODE))
        {
            delete m_PeakFile;
            m_PeakFile = NULL;
            m_bWritePeakFile = false;
        } else {
            m_PeakFile->Write(&m_PeakFileHeader, sizeof(m_PeakFileHeader));
            
            if(m_PeakCalcBuffer)
                delete[] m_PeakCalcBuffer;
            m_PeakCalcBufferSize = (m_SamplePerPeak * (m_OutputWF->wBitsPerSample / 8) * m_OutputWF->nChannels);
            m_PeakCalcBuffer = new char[m_PeakCalcBufferSize];
            DebugLog(TEXT("CWavWriterFilter::InitPeakData m_PeakCalcBufferSize=%d"), m_PeakCalcBufferSize);
            
            if(m_PeakCircBuffer)
                delete m_PeakCircBuffer;
            m_PeakCircBuffer = new CircBuffer( max(m_InputAllocatorBuffSize * 2, m_PeakCalcBufferSize * 2) );
        }
    }
    return S_OK;
}

//----------------------------------------------------------------------------

void CWavWriterFilter::PeakProcessing(BYTE* pInData, LONG lInLength, bool finalize)
{
    int BytesToRead = 0, BytesAvailable = 0;
    
    if(!m_bWritePeakFile || m_PeakFile == NULL)
        return;

    if (lInLength > m_PeakCircBuffer->Size()) {
        m_PeakCircBuffer->ChangeSize(lInLength * 2);
    }
    
    if(pInData != NULL)
        m_PeakCircBuffer->Write(pInData, lInLength);
    
    while( ((BytesAvailable = m_PeakCircBuffer->Filled()) >= m_PeakCalcBufferSize) || finalize)
    {
        if(finalize && (BytesAvailable < m_PeakCalcBufferSize))
        {
            ZeroMemory(m_PeakCalcBuffer, m_PeakCalcBufferSize);
            BytesToRead = BytesAvailable;
            finalize = false;
        } else {
            BytesToRead = m_PeakCalcBufferSize;
        }
        
        m_PeakCircBuffer->Read(m_PeakCalcBuffer, BytesToRead);
        
        switch(m_OutputWF->wBitsPerSample)
        {
        case 8:
            {           
                unsigned char *src = (unsigned char*) m_PeakCalcBuffer;
                unsigned char maxValue = 0;
                unsigned char minValue = 255;
                for(unsigned int i = 0; i < BytesToRead / sizeof(char); ++i)
                {
                    if(src[i] > maxValue)
                        maxValue = src[i];
                    if(src[i] < minValue)
                        minValue = src[i];
                }
                m_Peak.Max = (short)((maxValue - 128) << 8);
                m_Peak.Min = (short)((minValue - 128) << 8);
                m_PeakFile->Write(&m_Peak, sizeof(m_Peak));
                m_PeakFileHeader.PeakTabLen++;
            }
            break;
        case 16:
            {           
                short *src = (short*) m_PeakCalcBuffer;
                short maxValue = -32768;
                short minValue = 32767;
                for(unsigned int i = 0; i < BytesToRead / sizeof(short) ; ++i)
                {
                    if(src[i] > maxValue)
                        maxValue = src[i];
                    if(src[i] < minValue)
                        minValue = src[i];
                }
                m_Peak.Max = maxValue;
                m_Peak.Min = minValue;
                m_PeakFile->Write(&m_Peak, sizeof(m_Peak));
                m_PeakFileHeader.PeakTabLen++;
            }
            break;
        case 32:
            if (m_IsPCM) {
                int *src = (int*) m_PeakCalcBuffer;
                int maxValue = 0x80000000;
                int minValue = 0x7fffffff;
                for (unsigned int i = 0; i < BytesToRead / sizeof(int) ; ++i)
                {
                    if(src[i] > maxValue)
                        maxValue = src[i];
                    if(src[i] < minValue)
                        minValue = src[i];
                }
                m_Peak.Max = (short)(maxValue >> 16);
                m_Peak.Min = (short)(minValue >> 16);
                m_PeakFile->Write(&m_Peak, sizeof(m_Peak));
                m_PeakFileHeader.PeakTabLen++;
            } else {
                float *src = (float*) m_PeakCalcBuffer;
                float maxValue = -1.0;
                float minValue = 1.0;
                for (unsigned int i = 0; i < BytesToRead / sizeof(float) ; ++i)
                {
                    if(src[i] > maxValue)
                        maxValue = src[i];
                    if(src[i] < minValue)
                        minValue = src[i];
                }
                m_Peak.Max = float2short(maxValue);
                m_Peak.Min = float2short(minValue);
                m_PeakFile->Write(&m_Peak, sizeof(m_Peak));
                m_PeakFileHeader.PeakTabLen++;
            }
            break;
        }
    }
}

//----------------------------------------------------------------------------

void CWavWriterFilter::CleanPeakData()
{
    if(m_PeakFile)
    {
        // Process data left in m_DataBuffer
        PeakProcessing(NULL,0,true);
        // Update length in ms
        double LengthMs = (double)(m_cbWavData / m_InputChannels / (m_InputBitsPerSample / 8)) / m_InputSampleRate;
        m_PeakFileHeader.LengthMs = (int)(LengthMs * 1000 + 0.5);
        m_PeakFile->Seek(0, FILE_BEGIN);
        m_PeakFile->Write(&m_PeakFileHeader, sizeof(m_PeakFileHeader));
        delete m_PeakFile;
        m_PeakFile = NULL;
        delete[] m_PeakCalcBuffer;
        m_PeakCalcBuffer = NULL;
        delete m_PeakCircBuffer;
        m_PeakCircBuffer = NULL;
    }
}

//-----------------------------------------------------------------------------
// IFileSinkFilter
//-----------------------------------------------------------------------------

STDMETHODIMP CWavWriterFilter::SetFileName(LPCOLESTR pszFileName,const AM_MEDIA_TYPE *pmt)
{
    // Is this a valid filename supplied
    
    CheckPointer(pszFileName, E_POINTER);
    if(wcslen(pszFileName) > MAX_PATH)
        return ERROR_FILENAME_EXCED_RANGE;
    
    // Take a copy of the filename
    
    m_pFileName = new WCHAR[1 + lstrlenW(pszFileName)];
    if (m_pFileName == 0)
        return E_OUTOFMEMORY;
    
    lstrcpyW(m_pFileName, pszFileName);
        
    return S_OK;
    
} // SetFileName

//-----------------------------------------------------------------------------

STDMETHODIMP CWavWriterFilter::GetCurFile(LPOLESTR * ppszFileName, AM_MEDIA_TYPE *pmt)
{
    CheckPointer(ppszFileName, E_POINTER);
    *ppszFileName = NULL;
    
    if (m_pFileName != NULL) 
    {
        *ppszFileName = (LPOLESTR)
            QzTaskMemAlloc(sizeof(WCHAR) * (1+lstrlenW(m_pFileName)));
        
        if (*ppszFileName != NULL) 
        {
            lstrcpyW(*ppszFileName, m_pFileName);
        }
    }
    
    if (pmt) 
    {
        ZeroMemory(pmt, sizeof(*pmt));      
        pmt->majortype = MEDIATYPE_Audio;
        pmt->subtype = MEDIASUBTYPE_PCM;
    }

    return S_OK;
    
} // GetCurFile

// ----------------------------------------------------------------------------
// IMediaSeeking
// ----------------------------------------------------------------------------

STDMETHODIMP CWavWriterFilter::GetCapabilities( DWORD * pCapabilities )
{
    CheckPointer(pCapabilities, E_POINTER);
    *pCapabilities = m_dwSeekingCaps;
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::CheckCapabilities( DWORD * pCapabilities )
{
    CheckPointer(pCapabilities, E_POINTER);
    // make sure all requested capabilities are in our mask
    return (~m_dwSeekingCaps & *pCapabilities) ? S_FALSE : S_OK;
}

STDMETHODIMP CWavWriterFilter::IsFormatSupported(const GUID * pFormat)
{
    CheckPointer(pFormat, E_POINTER);
    // only seeking in time (REFERENCE_TIME units) is supported
    return *pFormat == TIME_FORMAT_MEDIA_TIME ? S_OK : S_FALSE;
}

STDMETHODIMP CWavWriterFilter::QueryPreferredFormat(GUID *pFormat)
{
    CheckPointer(pFormat, E_POINTER);
    *pFormat = TIME_FORMAT_MEDIA_TIME;
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::SetTimeFormat(const GUID * pFormat)
{
    CheckPointer(pFormat, E_POINTER);
    // nothing to set; just check that it's TIME_FORMAT_TIME
    return *pFormat == TIME_FORMAT_MEDIA_TIME ? S_OK : E_INVALIDARG;
}

STDMETHODIMP CWavWriterFilter::IsUsingTimeFormat(const GUID * pFormat)
{
    CheckPointer(pFormat, E_POINTER);
    return *pFormat == TIME_FORMAT_MEDIA_TIME ? S_OK : S_FALSE;
}

STDMETHODIMP CWavWriterFilter::GetTimeFormat(GUID *pFormat)
{
    CheckPointer(pFormat, E_POINTER);
    *pFormat = TIME_FORMAT_MEDIA_TIME;
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::GetDuration(LONGLONG *pDuration)
{
    CheckPointer(pDuration, E_POINTER);
    *pDuration = m_rtDuration;
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::GetCurrentPosition(LONGLONG *pCurrent)
{
    CheckPointer(pCurrent, E_POINTER);

    CMediaType mt;
    m_pPin->ConnectionMediaType(&mt);

    WAVEFORMATEX *wf = NULL;
    if(mt.FormatLength() >= sizeof(WAVEFORMATEX))
    {
        wf = (WAVEFORMATEX *)mt.Format();
    }
    
    if(wf)
    {       
        *pCurrent = (((LONGLONG)m_cbWavData / wf->nBlockAlign) / wf->nSamplesPerSec) * 10000000;
    } else {
        *pCurrent = 0;
    }
    
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::GetStopPosition(LONGLONG *pStop) { return E_NOTIMPL; }
STDMETHODIMP CWavWriterFilter::ConvertTimeFormat(LONGLONG* pTarget, const GUID* pTargetFormat, LONGLONG Source, const GUID* pSourceFormat) { return E_NOTIMPL; }
STDMETHODIMP CWavWriterFilter::SetPositions(LONGLONG* pCurrent, DWORD dwCurrentFlags, LONGLONG* pStop, DWORD dwStopFlags) { return E_NOTIMPL; }
STDMETHODIMP CWavWriterFilter::GetPositions(LONGLONG* pCurrent, LONGLONG* pStop) { return E_NOTIMPL; }
STDMETHODIMP CWavWriterFilter::GetAvailable(LONGLONG* pEarliest, LONGLONG* pLatest) { return E_NOTIMPL; }
STDMETHODIMP CWavWriterFilter::SetRate(double dRate) { return E_NOTIMPL; }
STDMETHODIMP CWavWriterFilter::GetRate(double* pdRate) { return E_NOTIMPL; }
STDMETHODIMP CWavWriterFilter::GetPreroll(LONGLONG* pllPreroll) { return E_NOTIMPL; }

// ----------------------------------------------------------------------------
// IWavWriter
// ----------------------------------------------------------------------------

STDMETHODIMP CWavWriterFilter::SetFastConversionMode(DWORD FastConvertMode)
{
    m_bFastConvertMode = FastConvertMode ? true : false;
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::GetFastConversionMode(DWORD *FastConvertMode)
{   
    CheckPointer(FastConvertMode, E_POINTER);
    *FastConvertMode = m_bFastConvertMode ? 1 : 0;
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::SetSamplesPerPeakRatio(DWORD SamplePerPeakRatio)
{
    m_SamplePerPeakRatio = SamplePerPeakRatio;
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::SetWritePeakFile(DWORD WritePeakFile)
{
    m_bWritePeakFile = WritePeakFile ? true : false;
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::SetWriteWavFile(DWORD WriteWavFile)
{
    m_bWriteWavFile = WriteWavFile ? true : false;
    return S_OK;
}

STDMETHODIMP CWavWriterFilter::SetPeakFileName(LPCOLESTR pszFileName)
{   
    CheckPointer(pszFileName, E_POINTER);
    if(wcslen(pszFileName) > MAX_PATH)
        return ERROR_FILENAME_EXCED_RANGE;
    
    // Take a copy of the filename
    
    m_pPeakFileName = new WCHAR[1 + lstrlenW(pszFileName)];
    if (m_pPeakFileName == 0)
        return E_OUTOFMEMORY;
    
    lstrcpyW(m_pPeakFileName, pszFileName);
    
    return S_OK;
}

//=============================================================================

CWavWriterInputPin::CWavWriterInputPin(CWavWriterFilter *pFilter,
                             CCritSec *pLock,
                             CCritSec *pReceiveLock,
                             HRESULT *phr) :

    CRenderedInputPin(NAME("CWavWriterInputPin"),
                  pFilter,                   // Filter
                  pLock,                     // Locking
                  phr,                       // Return code
                  L"Input"),                 // Pin name
    m_pReceiveLock(pReceiveLock),
    m_pFilter(pFilter)
{

}

//-----------------------------------------------------------------------------
//
// CheckMediaType
//
// Check if the pin can support this specific proposed type and format
//
HRESULT CWavWriterInputPin::CheckMediaType(const CMediaType *mtIn)
{
// 00000001-0000-0010-8000-00AA00389B71            MEDIASUBTYPE_PCM
    
    if(mtIn->formattype == FORMAT_WaveFormatEx)
    {
        WAVEFORMATEX *wf = NULL;
        WAVEFORMATEXTENSIBLE *wfe = NULL;
        ULONG formatLen = mtIn->FormatLength();
        if(formatLen >= sizeof(WAVEFORMATEX)) // 18
        {
            wf = (WAVEFORMATEX *) mtIn->Format();
        }
        if(formatLen >= sizeof(WAVEFORMATEXTENSIBLE) && wf->wFormatTag == WAVE_FORMAT_EXTENSIBLE) { //40
            wf = &((WAVEFORMATEXTENSIBLE*) mtIn->Format())->Format;
            wfe = (WAVEFORMATEXTENSIBLE*) mtIn->Format();
        }

		if (wf->wFormatTag == WAVE_FORMAT_PCM &&
			(wf->wBitsPerSample == 8 || wf->wBitsPerSample == 16 || wf->wBitsPerSample == 32)) {
			return S_OK;
		}
		if (wf->wFormatTag == WAVE_FORMAT_IEEE_FLOAT && wf->wBitsPerSample == 32) {
			return S_OK;
		}
        if(formatLen >= sizeof(WAVEFORMATEXTENSIBLE) && wf->wFormatTag == WAVE_FORMAT_EXTENSIBLE) {
            if(wfe->SubFormat == MEDIASUBTYPE_PCM &&
                (wf->wBitsPerSample == 8 || wf->wBitsPerSample == 16 || wf->wBitsPerSample == 32)) {
                return S_OK;
            }
            if(wfe->SubFormat == MEDIASUBTYPE_IEEE_FLOAT && wf->wBitsPerSample == 32) {
                return S_OK;
            }
        }
    }
    return S_FALSE;
}

//-----------------------------------------------------------------------------

HRESULT CWavWriterInputPin::CompleteConnect(IPin *pReceivePin)
{
    IMediaSeeking *pIMediaSeeking = NULL;
    HRESULT hr;
    
    // Get duration
    m_pFilter->m_rtDuration = 0;
    hr = pReceivePin->QueryInterface(IID_IMediaSeeking, (void **)&pIMediaSeeking);      
    if(SUCCEEDED(hr))
    {           
        pIMediaSeeking->GetDuration(&m_pFilter->m_rtDuration);
		pIMediaSeeking->Release();
    }
    
    // Keep info on input media type, and set info for output
    hr = pReceivePin->ConnectionMediaType(&m_pFilter->m_OutType);
    
    m_pFilter->m_OutputWF = NULL;
    if(m_pFilter->m_OutType.FormatLength() >= sizeof(WAVEFORMATEX))
    {
        m_pFilter->m_OutputWF = (WAVEFORMATEX *) m_pFilter->m_OutType.Format();
        m_pFilter->m_IsPCM = (m_pFilter->m_OutputWF->wFormatTag == WAVE_FORMAT_PCM) ? 1 : 0;
    }
    if(m_pFilter->m_OutType.FormatLength() >= sizeof(WAVEFORMATEXTENSIBLE) && m_pFilter->m_OutputWF->wFormatTag == WAVE_FORMAT_EXTENSIBLE) {
        WAVEFORMATEXTENSIBLE* wfe = (WAVEFORMATEXTENSIBLE*) m_pFilter->m_OutType.Format();
        m_pFilter->m_OutputWF = &wfe->Format;
        if(wfe->SubFormat == MEDIASUBTYPE_PCM) {
            m_pFilter->m_IsPCM = 1;
        } else if(wfe->SubFormat == MEDIASUBTYPE_IEEE_FLOAT) {
            m_pFilter->m_IsPCM = 0;
        }
    }
    
    m_pFilter->m_InputSampleRate = m_pFilter->m_OutputWF->nSamplesPerSec;
    m_pFilter->m_InputChannels = m_pFilter->m_OutputWF->nChannels;
	m_pFilter->m_InputBitsPerSample = m_pFilter->m_OutputWF->wBitsPerSample;

    if(m_pFilter->m_bFastConvertMode)
    {
        m_pFilter->m_OutputWF->nSamplesPerSec /= 2;
        m_pFilter->m_OutputWF->nChannels = 1;
		if (m_pFilter->m_OutputWF->wBitsPerSample == 32) {
			m_pFilter->m_OutputWF->wBitsPerSample = 16;
			m_pFilter->m_OutputWF->wFormatTag = WAVE_FORMAT_PCM;
		}
        m_pFilter->m_OutputWF->nBlockAlign = (WORD)((m_pFilter->m_OutputWF->wBitsPerSample / 8) * m_pFilter->m_OutputWF->nChannels);
        m_pFilter->m_OutputWF->nAvgBytesPerSec = m_pFilter->m_OutputWF->nBlockAlign * m_pFilter->m_OutputWF->nSamplesPerSec;
    }

    return CRenderedInputPin::CompleteConnect(pReceivePin);
}

//-----------------------------------------------------------------------------
//
// ReceiveCanBlock
//
// We don't hold up source threads on Receive
//
STDMETHODIMP CWavWriterInputPin::ReceiveCanBlock()
{
    return S_FALSE;
}

//-----------------------------------------------------------------------------
//
// Receive
//
// Do something with this media sample
//
STDMETHODIMP CWavWriterInputPin::Receive(IMediaSample *pSample)
{
    CheckPointer(pSample,E_POINTER);

    CAutoLock lock(m_pReceiveLock);
    PBYTE pbData;

    REFERENCE_TIME rtStart = 0, rtStop = 0;
    pSample->GetTime(&rtStart,&rtStop);

	// TODO : support dynamic media type change at start

    if((rtStart != m_tPrevStop) &&
        ((m_tPrevStart != rtStart) && (m_tPrevStop != rtStop)) &&
        (m_tPrevStop == 0))
    {
        // Hole in timestamps, pad with silence
        double NbSilentSamples = (double)(((rtStart - m_tPrevStop) * m_pFilter->m_InputSampleRate *
            m_pFilter->m_InputChannels) / 10000000);
        int SilentSamplesLen = (int)(NbSilentSamples * (m_pFilter->m_OutputWF->wBitsPerSample / 8));
        SilentSamplesLen += (SilentSamplesLen % (m_pFilter->m_InputChannels * m_pFilter->m_OutputWF->wBitsPerSample / 8));

        PBYTE DummyBuffer = new BYTE[m_pFilter->m_InputAllocatorBuffSize];
        ZeroMemory(DummyBuffer, m_pFilter->m_InputAllocatorBuffSize);
        int toWriteNow = 0;
        while(SilentSamplesLen > 0)
        {
            toWriteNow = min(m_pFilter->m_InputAllocatorBuffSize, SilentSamplesLen);
            m_pFilter->Write(DummyBuffer, toWriteNow);
            SilentSamplesLen -= toWriteNow;
        }
        delete[] DummyBuffer;
    }

    m_tPrevStart = rtStart;
    m_tPrevStop = rtStop;

    // Copy the data to the file
    HRESULT hr = pSample->GetPointer(&pbData);
    if (FAILED(hr)) {
        return hr;
    }

    return m_pFilter->Write(pbData, pSample->GetActualDataLength());
}

//-----------------------------------------------------------------------------

STDMETHODIMP CWavWriterInputPin::NewSegment(REFERENCE_TIME tStart,
                                       REFERENCE_TIME tStop,
                                       double dRate)
{
    m_tPrevStart = 0;
    m_tPrevStop = 0;
    return S_OK;
    
} // NewSegment

//-----------------------------------------------------------------------------
//
// EndOfStream
//
STDMETHODIMP CWavWriterInputPin::EndOfStream(void)
{
    CAutoLock lock(m_pReceiveLock);
    return CRenderedInputPin::EndOfStream();

} // EndOfStream

////////////////////////////////////////////////////////////////////////
//
// Exported entry points for registration and un-registration 
// (in this case they only call through to default implementations).
//
////////////////////////////////////////////////////////////////////////

//
// DllRegisterSever
//
// Handle the registration of this filter
//
STDAPI DllRegisterServer()
{
    return AMovieDllRegisterServer2( TRUE );

} // DllRegisterServer


//
// DllUnregisterServer
//
STDAPI DllUnregisterServer()
{
    return AMovieDllRegisterServer2( FALSE );

} // DllUnregisterServer


//
// DllEntryPoint
//
extern "C" BOOL WINAPI DllEntryPoint(HINSTANCE, ULONG, LPVOID);

BOOL APIENTRY DllMain(HANDLE hModule, 
                      DWORD  dwReason, 
                      LPVOID lpReserved)
{
    return DllEntryPoint((HINSTANCE)(hModule), dwReason, lpReserved);
}

