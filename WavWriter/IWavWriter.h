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

#ifndef _IWAVWRITER_H_
#define _IWAVWRITER_H_

#ifdef __cplusplus
extern "C" {
#endif

// {3B7A03CE-3D3B-4fef-8CDE-3D6C2709AFF1}
DEFINE_GUID(IID_IWavWriter,
			0x3b7a03ce, 0x3d3b, 0x4fef, 0x8c, 0xde, 0x3d, 0x6c, 0x27, 0x9, 0xaf, 0xf1);

//
// IWavWriter
//
DECLARE_INTERFACE_(IWavWriter, IUnknown) {
	STDMETHOD(SetFastConversionMode)(DWORD FastConvertMode) = 0;
	STDMETHOD(GetFastConversionMode)(DWORD *FastConvertMode) = 0;
	STDMETHOD(SetSamplesPerPeakRatio)(DWORD SamplePerPeakRatio) = 0;
	STDMETHOD(SetWritePeakFile)(DWORD WritePeakFile) = 0;
	STDMETHOD(SetPeakFileName)(LPCOLESTR pszFileName) = 0;
	STDMETHOD(SetWriteWavFile)(DWORD WriteWavFile) = 0;
};
	
#ifdef __cplusplus
}
#endif

// -----------------------------------------------------------------------------
#endif // _IWAVWRITER_H_
// -----------------------------------------------------------------------------