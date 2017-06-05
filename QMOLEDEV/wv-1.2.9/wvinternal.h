#ifndef WV_INTERNAL
#define WV_INTERNAL
unsigned int wvMapNameToTokenType (const char * name);

void internal_wvReleasePAPX_FKP (PAPX_FKP * fkp);
void internal_wvReleaseCHPX_FKP (CHPX_FKP * fkp);

void wvGetBRC_internal (BRC * abrc, wvStream * infd, U8 * pointer);
void wvGetBRC10_internal (BRC10 * item, wvStream * infd, U8 * pointer);
void wvGetDCS_internal (DCS * item, wvStream * fd, U8 * pointer);
void wvGetDTTM_internal (DTTM *, wvStream * fd, U8 * pointer);
void wvGetNUMRM_internal (NUMRM * item, wvStream * fd, U8 * pointer);
void wvGetSHD_internal (SHD * item, wvStream * fd, U8 * pointer);
void wvGetBRC_internal6 (BRC * abrc, wvStream * infd, U8 * pointer);
void wvGetTBD_internal (TBD * item, wvStream * fd, U8 * pointer);
void wvGetOLST_internal (wvVersion ver, OLST * item, wvStream * fd,
			 U8 * pointer);
void wvGetANLV_internal (ANLV * item, wvStream * fd, U8 * pointer);
int wvGetTC_internal (wvVersion ver, TC * tc, wvStream * infd, U8 * pointer);
void wvGetTLP_internal (TLP * item, wvStream * infd, U8 * pointer);

/* simple stream creation and writing functions */
int wvStream_write (void *ptr, size_t size, size_t nmemb, wvStream * in);
int write_32ubit (wvStream * in, U32 out);
int write_16ubit (wvStream * in, U16 out);
int write_8ubit (wvStream * in, U8 out);

void wvPutFBSE (FBSE * item, wvStream * fd);

void wvPutFAnchor (FAnchor * item, wvStream * fd);
void wvPutFOPTE (FOPTE * afopte, wvStream * fd);
void wvPutFOPTEArray (FOPTE ** fopte, MSOFBH * msofbh, wvStream * fd);

#endif
