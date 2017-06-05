/* 
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2012 Colin Leroy <colin@colino.net> and 
 * the Claws Mail team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#include "claws-features.h"
#endif

#ifdef USE_GPGME

#include "defs.h"
#include <glib.h>
#include <gpgme.h>
#include <ctype.h>
#include <glib/gi18n.h>

#include "utils.h"
#include "privacy.h"
#include "procmime.h"

#include "smime.h"
#include <plugins/pgpcore/sgpgme.h>
#include <plugins/pgpcore/prefs_gpg.h>
#include <plugins/pgpcore/passphrase.h>

#include "alertpanel.h"
#include "prefs_common.h"
#include "procmime.h"
#include "plugin.h"

typedef struct _PrivacyDataPGP PrivacyDataPGP;

struct _PrivacyDataPGP
{
	PrivacyData	data;
	
	gboolean	done_sigtest;
	gboolean	is_signed;
	gpgme_verify_result_t	sigstatus;
	gpgme_ctx_t 	ctx;
};

static PrivacySystem smime_system;

static gint smime_check_signature(MimeInfo *mimeinfo);

static PrivacyDataPGP *smime_new_privacydata()
{
	PrivacyDataPGP *data;

	data = g_new0(PrivacyDataPGP, 1);
	data->data.system = &smime_system;
	data->done_sigtest = FALSE;
	data->is_signed = FALSE;
	data->sigstatus = NULL;
	gpgme_new(&data->ctx);
	
	return data;
}

static void smime_free_privacydata(PrivacyData *_data)
{
	PrivacyDataPGP *data = (PrivacyDataPGP *) _data;
	gpgme_release(data->ctx);
	g_free(data);
}

static gboolean smime_is_signed(MimeInfo *mimeinfo)
{
	MimeInfo *parent;
	MimeInfo *signature;
	const gchar *protocol, *tmpstr;
	PrivacyDataPGP *data = NULL;
	
	cm_return_val_if_fail(mimeinfo != NULL, FALSE);
	if (mimeinfo->privacy != NULL) {
		data = (PrivacyDataPGP *) mimeinfo->privacy;
		if (data->done_sigtest)
			return data->is_signed;
	}
	
	if (!g_ascii_strcasecmp(mimeinfo->subtype, "pkcs7-mime") ||
	    !g_ascii_strcasecmp(mimeinfo->subtype, "x-pkcs7-mime")) {
		tmpstr = procmime_mimeinfo_get_parameter(mimeinfo, "smime-type");
		if (tmpstr && !g_ascii_strcasecmp(tmpstr, "signed-data")) {
			if (data == NULL) {
				data = smime_new_privacydata();
				mimeinfo->privacy = (PrivacyData *) data;
			}

			data->done_sigtest = TRUE;
			data->is_signed = TRUE;
			smime_check_signature(mimeinfo);
			return TRUE;
		}
	}

	/* check parent */
	parent = procmime_mimeinfo_parent(mimeinfo);
	if (parent == NULL)
		return FALSE;
	
	if ((parent->type != MIMETYPE_MULTIPART) ||
	    g_ascii_strcasecmp(parent->subtype, "signed"))
		return FALSE;
	protocol = procmime_mimeinfo_get_parameter(parent, "protocol");
	if ((protocol == NULL) || 
	    (g_ascii_strcasecmp(protocol, "application/pkcs7-signature") &&
	     g_ascii_strcasecmp(protocol, "application/x-pkcs7-signature")))
		return FALSE;

	/* check if mimeinfo is the first child */
	if (parent->node->children->data != mimeinfo)
		return FALSE;


	/* check signature */
	signature = parent->node->children->next != NULL ? 
	    (MimeInfo *) parent->node->children->next->data : NULL;
	if (signature == NULL)
		return FALSE;
	if ((signature->type != MIMETYPE_APPLICATION) ||
	    (g_ascii_strcasecmp(signature->subtype, "pkcs7-signature") &&
	     g_ascii_strcasecmp(signature->subtype, "x-pkcs7-signature")))
		return FALSE;

	if (data == NULL) {
		data = smime_new_privacydata();
		mimeinfo->privacy = (PrivacyData *) data;
	}
	
	data->done_sigtest = TRUE;
	data->is_signed = TRUE;

	return TRUE;
}

static gchar *get_canonical_content(FILE *fp, const gchar *boundary)
{
	gchar *ret;
	GString *textbuffer;
	guint boundary_len = 0;
	gchar buf[BUFFSIZE];

	if (boundary) {
		boundary_len = strlen(boundary);
		while (fgets(buf, sizeof(buf), fp) != NULL)
			if (IS_BOUNDARY(buf, boundary, boundary_len))
				break;
	}
	
	textbuffer = g_string_new("");
	while (fgets(buf, sizeof(buf), fp) != NULL) {
		gchar *buf2;

		if (boundary && IS_BOUNDARY(buf, boundary, boundary_len))
			break;
		
		buf2 = canonicalize_str(buf);
		g_string_append(textbuffer, buf2);
		g_free(buf2);
	}
	g_string_truncate(textbuffer, textbuffer->len - 2);
		
	ret = textbuffer->str;
	g_string_free(textbuffer, FALSE);

	return ret;
}

static gint smime_check_signature(MimeInfo *mimeinfo)
{
	PrivacyDataPGP *data;
	MimeInfo *parent, *signature;
	FILE *fp;
	gchar *boundary;
	gchar *textstr = NULL;
	const gchar *tmpstr;
	gpgme_data_t sigdata = NULL, textdata = NULL;
	gpgme_error_t err;
	cm_return_val_if_fail(mimeinfo != NULL, -1);
	cm_return_val_if_fail(mimeinfo->privacy != NULL, -1);
	data = (PrivacyDataPGP *) mimeinfo->privacy;
	gpgme_new(&data->ctx);
	EncodingType oldenc = ENC_BINARY;
	
	debug_print("Checking S/MIME signature\n");

	err = gpgme_set_protocol(data->ctx, GPGME_PROTOCOL_CMS);

	if (err) {
		debug_print ("gpgme_set_protocol failed: %s\n",
                   gpgme_strerror (err));
	}
	parent = procmime_mimeinfo_parent(mimeinfo);

	fp = g_fopen(parent->data.filename, "rb");
	cm_return_val_if_fail(fp != NULL, SIGNATURE_INVALID);
	
	boundary = g_hash_table_lookup(parent->typeparameters, "boundary");
	if (!boundary) {
		gchar *tmpfile = get_tmp_file();
		debug_print("no boundary\n");
		if (tmpfile) {
			if (mimeinfo->encoding_type != ENC_BASE64) {
				procmime_encode_content(mimeinfo, ENC_BASE64);
			}
			oldenc = mimeinfo->encoding_type;
			if (mimeinfo->encoding_type == ENC_BASE64)
				mimeinfo->encoding_type = ENC_BINARY;
			if (procmime_get_part(tmpfile, mimeinfo) == 0) {
				textstr = file_read_to_str(tmpfile);
			} else {
				textstr = NULL;
			}
			if (mimeinfo->encoding_type != oldenc)
				mimeinfo->encoding_type = oldenc;
		}
		g_free(tmpfile);
	} else {
		textstr = get_canonical_content(fp, boundary);
	}
	err = gpgme_data_new_from_mem(&textdata, textstr, textstr?strlen(textstr):0, 0);
	
	if (err) {
		debug_print ("gpgme_data_new_from_mem failed: %s\n",
                   gpgme_strerror (err));
	}

	if (!g_ascii_strcasecmp(mimeinfo->subtype, "pkcs7-mime") ||
	    !g_ascii_strcasecmp(mimeinfo->subtype, "x-pkcs7-mime")) {
		tmpstr = procmime_mimeinfo_get_parameter(mimeinfo, "smime-type");
		if (tmpstr && !g_ascii_strcasecmp(tmpstr, "signed-data")) {
			gpgme_data_t cipher;
			size_t len;
			if (oldenc == ENC_BASE64)
				gpgme_data_set_encoding (textdata, GPGME_DATA_ENCODING_BASE64);
			gpgme_data_new(&cipher);
			data->sigstatus =
				sgpgme_verify_signature	(data->ctx, textdata, NULL, cipher);
			gpgme_data_release(textdata);
			g_free(textstr);
			cm_gpgme_data_rewind(cipher);
			textstr = sgpgme_data_release_and_get_mem(cipher, &len);
			fclose(fp);
			if (textstr && len > 0)
				textstr[len-1]='\0';

			if (textstr && len) {
				gchar *tmp_file = get_tmp_file();
				MimeInfo *newinfo = NULL, *decinfo = NULL, *parentinfo = NULL;

				str_write_to_file(textstr, tmp_file);
				newinfo = procmime_scan_file(tmp_file);
				decinfo = g_node_first_child(newinfo->node) != NULL ?
					g_node_first_child(newinfo->node)->data : NULL;

				if (decinfo == NULL)
					return -1;

				g_node_unlink(decinfo->node);
				procmime_mimeinfo_free_all(newinfo);
				decinfo->tmp = TRUE;
				parentinfo = procmime_mimeinfo_parent(mimeinfo);

				if (parentinfo->type == MIMETYPE_MESSAGE && 
				    !strcmp(parentinfo->subtype, "rfc822")) {
					procmime_decode_content(parentinfo);
					procmime_encode_content(parentinfo, ENC_BASE64);
					procmime_encode_content(parentinfo, ENC_8BIT);
					if (parentinfo->content == MIMECONTENT_MEM) {
						gint newlen = 
							(gint)(strstr(parentinfo->data.mem, "\n\n") - parentinfo->data.mem);
						if (newlen > 0)
							parentinfo->length = newlen;
					}
				}
				g_node_prepend(parentinfo->node, decinfo->node);
				return 0;
			} else {
				return -1;
			}
		}
	}

	signature = (MimeInfo *) mimeinfo->node->next->data;
	sigdata = sgpgme_data_from_mimeinfo(signature);

	err = 0;
	if (signature->encoding_type == ENC_BASE64) {
		err = gpgme_data_set_encoding (sigdata, GPGME_DATA_ENCODING_BASE64);
	}
	
	if (err) {
		debug_print ("gpgme_data_set_encoding failed: %s\n",
			gpgme_strerror (err));
	}

	data->sigstatus =
		sgpgme_verify_signature	(data->ctx, sigdata, textdata, NULL);

	gpgme_data_release(sigdata);
	gpgme_data_release(textdata);
	g_free(textstr);
	fclose(fp);
	
	return 0;
}

static SignatureStatus smime_get_sig_status(MimeInfo *mimeinfo)
{
	PrivacyDataPGP *data = (PrivacyDataPGP *) mimeinfo->privacy;
	
	cm_return_val_if_fail(data != NULL, SIGNATURE_INVALID);

	if (data->sigstatus == NULL && 
	    prefs_gpg_get_config()->auto_check_signatures)
		smime_check_signature(mimeinfo);
	
	return sgpgme_sigstat_gpgme_to_privacy(data->ctx, data->sigstatus);
}

static gchar *smime_get_sig_info_short(MimeInfo *mimeinfo)
{
	PrivacyDataPGP *data = (PrivacyDataPGP *) mimeinfo->privacy;
	
	cm_return_val_if_fail(data != NULL, g_strdup("Error"));

	if (data->sigstatus == NULL && 
	    prefs_gpg_get_config()->auto_check_signatures)
		smime_check_signature(mimeinfo);
	
	return sgpgme_sigstat_info_short(data->ctx, data->sigstatus);
}

static gchar *smime_get_sig_info_full(MimeInfo *mimeinfo)
{
	PrivacyDataPGP *data = (PrivacyDataPGP *) mimeinfo->privacy;
	
	cm_return_val_if_fail(data != NULL, g_strdup("Error"));

	if (data->sigstatus == NULL && 
	    prefs_gpg_get_config()->auto_check_signatures)
		smime_check_signature(mimeinfo);
	
	return sgpgme_sigstat_info_full(data->ctx, data->sigstatus);
}

static gboolean smime_is_encrypted(MimeInfo *mimeinfo)
{
	const gchar *tmpstr;
	
	if (mimeinfo->type != MIMETYPE_APPLICATION)
		return FALSE;
	if (!g_ascii_strcasecmp(mimeinfo->subtype, "pkcs7-mime")) {
		tmpstr = procmime_mimeinfo_get_parameter(mimeinfo, "smime-type");
		if (tmpstr && g_ascii_strcasecmp(tmpstr, "enveloped-data"))
			return FALSE;
		else 
			return TRUE;

	} else if (!g_ascii_strcasecmp(mimeinfo->subtype, "x-pkcs7-mime")) {
		tmpstr = procmime_mimeinfo_get_parameter(mimeinfo, "smime-type");
		if (tmpstr && g_ascii_strcasecmp(tmpstr, "enveloped-data"))
			return FALSE;
		else 
			return TRUE;
	}
	return FALSE;
}

static MimeInfo *smime_decrypt(MimeInfo *mimeinfo)
{
	MimeInfo *encinfo, *decinfo, *parseinfo;
	gpgme_data_t cipher = NULL, plain = NULL;
	static gint id = 0;
	FILE *dstfp;
	gchar *fname;
	gpgme_verify_result_t sigstat = NULL;
	PrivacyDataPGP *data = NULL;
	gpgme_ctx_t ctx;
	gpgme_error_t err;
	gchar *chars;
	size_t len;

	cm_return_val_if_fail(smime_is_encrypted(mimeinfo), NULL);
	
	if ((err = gpgme_new(&ctx)) != GPG_ERR_NO_ERROR) {
		privacy_set_error(_("Couldn't initialize GPG context, %s"), gpgme_strerror(err));
		return NULL;
	}

	err = gpgme_set_protocol(ctx, GPGME_PROTOCOL_CMS);
	if (err) {
		debug_print ("gpgme_set_protocol failed: %s\n",
                   gpgme_strerror (err));
		privacy_set_error(_("Couldn't set GPG protocol, %s"), gpgme_strerror(err));
		gpgme_release(ctx);
		return NULL;
	}
	gpgme_set_armor(ctx, TRUE);

	encinfo = mimeinfo;

	cipher = sgpgme_data_from_mimeinfo(encinfo);
	
	plain = sgpgme_decrypt_verify(cipher, &sigstat, ctx);

	gpgme_data_release(cipher);
	if (plain == NULL) {
		debug_print("plain is null!\n");
		gpgme_release(ctx);
		return NULL;
	}

    	fname = g_strdup_printf("%s%cplaintext.%08x",
		get_mime_tmp_dir(), G_DIR_SEPARATOR, ++id);

    	if ((dstfp = g_fopen(fname, "wb")) == NULL) {
        	FILE_OP_ERROR(fname, "g_fopen");
        	g_free(fname);
        	gpgme_data_release(plain);
		gpgme_release(ctx);
		debug_print("can't open!\n");
		privacy_set_error(_("Couldn't open temporary file"));
		return NULL;
    	}

	if (fprintf(dstfp, "MIME-Version: 1.0\n") < 0) {
        	FILE_OP_ERROR(fname, "fprintf");
        	g_free(fname);
		fclose(dstfp);
        	gpgme_data_release(plain);
		gpgme_release(ctx);
		debug_print("can't close!\n");
		privacy_set_error(_("Couldn't write to temporary file"));
		return NULL;
	}

	chars = sgpgme_data_release_and_get_mem(plain, &len);

	if (len > 0) {
		if (fwrite(chars, 1, len, dstfp) < len) {
        		FILE_OP_ERROR(fname, "fwrite");
        		fclose(dstfp);
        		g_free(fname);
        		g_free(chars);
        		gpgme_data_release(plain);
			gpgme_release(ctx);
			debug_print("can't write!\n");
			privacy_set_error(_("Couldn't write to temporary file"));
			return NULL;
		}
	}
	if (fclose(dstfp) == EOF) {
        	FILE_OP_ERROR(fname, "fclose");
        	g_free(fname);
       		g_free(chars);
        	gpgme_data_release(plain);
		gpgme_release(ctx);
		debug_print("can't close!\n");
		privacy_set_error(_("Couldn't close temporary file"));
		return NULL;
	}
	g_free(chars);

	parseinfo = procmime_scan_file(fname);
	g_free(fname);
	if (parseinfo == NULL) {
		privacy_set_error(_("Couldn't parse decrypted file."));
		gpgme_release(ctx);
		return NULL;
	}
	decinfo = g_node_first_child(parseinfo->node) != NULL ?
		g_node_first_child(parseinfo->node)->data : NULL;
	if (decinfo == NULL) {
		privacy_set_error(_("Couldn't parse decrypted file parts."));
		gpgme_release(ctx);
		return NULL;
	}

	g_node_unlink(decinfo->node);
	procmime_mimeinfo_free_all(parseinfo);

	decinfo->tmp = TRUE;

	if (sigstat != NULL && sigstat->signatures != NULL) {
		if (decinfo->privacy != NULL) {
			data = (PrivacyDataPGP *) decinfo->privacy;
		} else {
			data = smime_new_privacydata();
			decinfo->privacy = (PrivacyData *) data;	
		}
		data->done_sigtest = TRUE;
		data->is_signed = TRUE;
		data->sigstatus = sigstat;
		if (data->ctx)
			gpgme_release(data->ctx);
		data->ctx = ctx;
	} else
		gpgme_release(ctx);
	
	
	
	return decinfo;
}

gboolean smime_sign(MimeInfo *mimeinfo, PrefsAccount *account, const gchar *from_addr)
{
	MimeInfo *msgcontent, *sigmultipart, *newinfo;
	gchar *textstr, *micalg = NULL;
	FILE *fp;
	gchar *boundary = NULL;
	gchar *sigcontent;
	gpgme_ctx_t ctx;
	gpgme_data_t gpgtext, gpgsig;
	gpgme_error_t err;
	size_t len;
	struct passphrase_cb_info_s info;
	gpgme_sign_result_t result = NULL;
	gchar *test_msg;
	gchar *real_content = NULL;
	
	fp = my_tmpfile();
	if (fp == NULL) {
		perror("my_tmpfile");
		return FALSE;
	}
	procmime_write_mimeinfo(mimeinfo, fp);
	rewind(fp);

	/* read temporary file into memory */
	test_msg = file_read_stream_to_str(fp);
	fclose(fp);
	
	memset (&info, 0, sizeof info);

	/* remove content node from message */
	msgcontent = (MimeInfo *) mimeinfo->node->children->data;
	g_node_unlink(msgcontent->node);

	/* create temporary multipart for content */
	sigmultipart = procmime_mimeinfo_new();
	sigmultipart->type = MIMETYPE_MULTIPART;
	sigmultipart->subtype = g_strdup("signed");
	
	do {
		if (boundary)
			g_free(boundary);
		boundary = generate_mime_boundary("Sig");
	} while (strstr(test_msg, boundary) != NULL);
	
	g_free(test_msg);

	g_hash_table_insert(sigmultipart->typeparameters, g_strdup("boundary"),
                            g_strdup(boundary));
	g_hash_table_insert(sigmultipart->typeparameters, g_strdup("protocol"),
                            g_strdup("application/pkcs7-signature"));
	g_node_append(sigmultipart->node, msgcontent->node);
	g_node_append(mimeinfo->node, sigmultipart->node);

	/* write message content to temporary file */
	fp = my_tmpfile();
	if (fp == NULL) {
		perror("my_tmpfile");
		return FALSE;
	}
	procmime_write_mimeinfo(sigmultipart, fp);
	rewind(fp);

	/* read temporary file into memory */
	textstr = get_canonical_content(fp, boundary);

	g_free(boundary);

	fclose(fp);

	gpgme_data_new_from_mem(&gpgtext, textstr, textstr?strlen(textstr):0, 0);
	gpgme_data_new(&gpgsig);
	gpgme_new(&ctx);
	gpgme_set_armor(ctx, TRUE);
	gpgme_signers_clear (ctx);

	err = gpgme_set_protocol(ctx, GPGME_PROTOCOL_CMS);

	if (err) {
		debug_print ("gpgme_set_protocol failed: %s\n",
                   gpgme_strerror (err));
		gpgme_data_release(gpgtext);
		gpgme_release(ctx);
		return FALSE;
	}

	if (!sgpgme_setup_signers(ctx, account, from_addr)) {
		debug_print("setup_signers failed\n");
		gpgme_data_release(gpgtext);
		gpgme_release(ctx);
		return FALSE;
	}

	info.c = ctx;
	prefs_gpg_enable_agent(TRUE);
    	gpgme_set_passphrase_cb (ctx, NULL, &info);
	
	err = gpgme_op_sign(ctx, gpgtext, gpgsig, GPGME_SIG_MODE_DETACH);
	if (err != GPG_ERR_NO_ERROR) {
		alertpanel_error("S/MIME : Cannot sign, %s (%d)", gpg_strerror(err), gpg_err_code(err));
		gpgme_data_release(gpgtext);
		gpgme_release(ctx);
		return FALSE;
	}
	result = gpgme_op_sign_result(ctx);
	if (result && result->signatures) {
	    if (gpgme_get_protocol(ctx) == GPGME_PROTOCOL_OpenPGP) {
		micalg = g_strdup_printf("PGP-%s", gpgme_hash_algo_name(
			    result->signatures->hash_algo));
	    } else {
		micalg = g_strdup(gpgme_hash_algo_name(
			    result->signatures->hash_algo));
	    }
	} else {
	    /* can't get result (maybe no signing key?) */
	    debug_print("gpgme_op_sign_result error\n");
	    return FALSE;
	}

	gpgme_release(ctx);
	sigcontent = sgpgme_data_release_and_get_mem(gpgsig, &len);
	gpgme_data_release(gpgtext);
	g_free(textstr);

	if (!sigcontent) {
		gpgme_release(ctx);
		g_free(micalg);
		return FALSE;
	}
	real_content = sigcontent+strlen("-----BEGIN SIGNED MESSAGE-----\n");
	if (!strstr(real_content, "-----END SIGNED MESSAGE-----")) {
		debug_print("missing end\n");
		gpgme_release(ctx);
		g_free(micalg);
		return FALSE;
	}
	*strstr(real_content, "-----END SIGNED MESSAGE-----") = '\0';
	/* add signature */
	g_hash_table_insert(sigmultipart->typeparameters, g_strdup("micalg"),
                            micalg);

	newinfo = procmime_mimeinfo_new();
	newinfo->type = MIMETYPE_APPLICATION;
	newinfo->subtype = g_strdup("pkcs7-signature");
	g_hash_table_insert(newinfo->typeparameters, g_strdup("name"),
			     g_strdup("smime.p7s"));
	newinfo->content = MIMECONTENT_MEM;
	newinfo->disposition = DISPOSITIONTYPE_ATTACHMENT;
	g_hash_table_insert(newinfo->dispositionparameters, g_strdup("filename"),
			    g_strdup("smime.p7s"));
	newinfo->data.mem = g_malloc(len + 1);
	g_memmove(newinfo->data.mem, real_content, len);
	newinfo->data.mem[len] = '\0';
	newinfo->encoding_type = ENC_BASE64;
	g_node_append(sigmultipart->node, newinfo->node);

	g_free(sigcontent);

	return TRUE;
}
gchar *smime_get_encrypt_data(GSList *recp_names)
{
	return sgpgme_get_encrypt_data(recp_names, GPGME_PROTOCOL_CMS);
}

static const gchar *smime_get_encrypt_warning(void)
{
	if (prefs_gpg_should_skip_encryption_warning(smime_system.id))
		return NULL;
	else
		return _("Please note that email headers, like Subject, "
			 "are not encrypted by the S/MIME system.");
}

static void smime_inhibit_encrypt_warning(gboolean inhibit)
{
	if (inhibit)
		prefs_gpg_add_skip_encryption_warning(smime_system.id);
	else
		prefs_gpg_remove_skip_encryption_warning(smime_system.id);
}

static gchar *fp_read_noconv(FILE *fp)
{
	GByteArray *array;
	guchar buf[BUFSIZ];
	gint n_read;
	gchar *result = NULL;

	if (!fp)
		return NULL;
	array = g_byte_array_new();

	while ((n_read = fread(buf, sizeof(gchar), sizeof(buf), fp)) > 0) {
		if (n_read < sizeof(buf) && ferror(fp))
			break;
		g_byte_array_append(array, buf, n_read);
	}

	if (ferror(fp)) {
		FILE_OP_ERROR("file stream", "fread");
		g_byte_array_free(array, TRUE);
		return NULL;
	}

	buf[0] = '\0';
	g_byte_array_append(array, buf, 1);
	result = (gchar *)array->data;
	g_byte_array_free(array, FALSE);
	
	return result;
}

gboolean smime_encrypt(MimeInfo *mimeinfo, const gchar *encrypt_data)
{
	MimeInfo *msgcontent, *encmultipart;
	FILE *fp;
	gchar *enccontent;
	size_t len;
	gchar *textstr = NULL;
	gpgme_data_t gpgtext = NULL, gpgenc = NULL;
	gpgme_ctx_t ctx = NULL;
	gpgme_key_t *kset = NULL;
	gchar **fprs = g_strsplit(encrypt_data, " ", -1);
	gint i = 0;
	gpgme_error_t err;
	gchar *tmpfile = NULL;

	while (fprs[i] && strlen(fprs[i])) {
		i++;
	}
	
	gpgme_new(&ctx);

	err = gpgme_set_protocol(ctx, GPGME_PROTOCOL_CMS);

	if (err) {
		debug_print ("gpgme_set_protocol failed: %s\n",
                   gpgme_strerror (err));
		return FALSE;   
	}

	kset = g_malloc(sizeof(gpgme_key_t)*(i+1));
	memset(kset, 0, sizeof(gpgme_key_t)*(i+1));
	i = 0;

	while (fprs[i] && strlen(fprs[i])) {
		gpgme_key_t key;
		gpgme_error_t err;
		err = gpgme_get_key(ctx, fprs[i], &key, 0);
		if (err) {
			debug_print("can't add key '%s'[%d] (%s)\n", fprs[i],i, gpgme_strerror(err));
			break;
		}
		debug_print("found %s at %d\n", fprs[i], i);
		kset[i] = key;
		i++;
	}
	
	debug_print("Encrypting message content\n");

	/* remove content node from message */
	msgcontent = (MimeInfo *) mimeinfo->node->children->data;
	g_node_unlink(msgcontent->node);


	/* create temporary multipart for content */
	encmultipart = procmime_mimeinfo_new();
	encmultipart->type = MIMETYPE_APPLICATION;
	encmultipart->subtype = g_strdup("x-pkcs7-mime");
	g_hash_table_insert(encmultipart->typeparameters, g_strdup("name"),
                            g_strdup("smime.p7m"));
	g_hash_table_insert(encmultipart->typeparameters,
			    g_strdup("smime-type"),
			    g_strdup("enveloped-data"));
	
	encmultipart->disposition = DISPOSITIONTYPE_ATTACHMENT;
	g_hash_table_insert(encmultipart->dispositionparameters, g_strdup("filename"),
                            g_strdup("smime.p7m"));

	g_node_append(encmultipart->node, msgcontent->node);

	/* write message content to temporary file */
	tmpfile = get_tmp_file();
	fp = g_fopen(tmpfile, "wb");
	if (fp == NULL) {
		perror("get_tmp_file");
		g_free(kset);
		return FALSE;
	}
	procmime_decode_content(msgcontent);
	procmime_write_mime_header(msgcontent, fp);
	procmime_write_mimeinfo(msgcontent, fp);
	fclose(fp);
	canonicalize_file_replace(tmpfile);
	fp = g_fopen(tmpfile, "rb");
	if (fp == NULL) {
		perror("get_tmp_file");
		g_free(kset);
		return FALSE;
	}
	g_free(tmpfile);

	/* read temporary file into memory */
	textstr = fp_read_noconv(fp);

	fclose(fp);

	/* encrypt data */
	gpgme_data_new_from_mem(&gpgtext, textstr, textstr?strlen(textstr):0, 0);
	gpgme_data_new(&gpgenc);
	cm_gpgme_data_rewind(gpgtext);
	
	gpgme_op_encrypt(ctx, kset, GPGME_ENCRYPT_ALWAYS_TRUST, gpgtext, gpgenc);

	gpgme_release(ctx);
	g_free(kset);
	enccontent = sgpgme_data_release_and_get_mem(gpgenc, &len);

	if (!enccontent) {
		g_warning("no enccontent\n");
		return FALSE;
	}

	tmpfile = get_tmp_file();
	fp = g_fopen(tmpfile, "wb");
	if (fp) {
		if (fwrite(enccontent, 1, len, fp) < len) {
			FILE_OP_ERROR(tmpfile, "fwrite");
			fclose(fp);
			claws_unlink(tmpfile);
			g_free(tmpfile);
			return FALSE;
		}
		if (fclose(fp) == EOF) {
			FILE_OP_ERROR(tmpfile, "fclose");
			claws_unlink(tmpfile);
			g_free(tmpfile);
			return FALSE;
		}
	} else {
		perror("get_tmp_file");
		g_free(tmpfile);
		return FALSE;
	}
	gpgme_data_release(gpgtext);
	g_free(textstr);

	/* create encrypted multipart */
	procmime_mimeinfo_free_all(msgcontent);
	g_node_append(mimeinfo->node, encmultipart->node);

	encmultipart->content = MIMECONTENT_FILE;
	encmultipart->data.filename = tmpfile;
	procmime_encode_content(encmultipart, ENC_BASE64);

	g_free(enccontent);

	return TRUE;
}

static PrivacySystem smime_system = {
	"smime",			/* id */
	"S-MIME",			/* name */

	smime_free_privacydata,	/* free_privacydata */

	smime_is_signed,		/* is_signed(MimeInfo *) */
	smime_check_signature,	/* check_signature(MimeInfo *) */
	smime_get_sig_status,		/* get_sig_status(MimeInfo *) */
	smime_get_sig_info_short,	/* get_sig_info_short(MimeInfo *) */
	smime_get_sig_info_full,	/* get_sig_info_full(MimeInfo *) */

	smime_is_encrypted,		/* is_encrypted(MimeInfo *) */
	smime_decrypt,			/* decrypt(MimeInfo *) */

	TRUE,
	smime_sign,

	TRUE,
	smime_get_encrypt_data,
	smime_encrypt,
	smime_get_encrypt_warning,
	smime_inhibit_encrypt_warning,
};

void smime_init()
{
	privacy_register_system(&smime_system);
}

void smime_done()
{
	privacy_unregister_system(&smime_system);
}

struct PluginFeature *plugin_provides(void)
{
	static struct PluginFeature features[] = 
		{ {PLUGIN_PRIVACY, N_("S/MIME")},
		  {PLUGIN_NOTHING, NULL}};
	return features;
}
#endif /* USE_GPGME */
