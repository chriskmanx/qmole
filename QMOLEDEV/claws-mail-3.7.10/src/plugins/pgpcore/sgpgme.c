/*
 * Claws Mail -- a GTK+ based, lightweight, and fast e-mail client
 * Copyright (C) 1999-2011 the Claws Mail team
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * 
 */
 
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
 
#ifdef USE_GPGME

#include <time.h>
#include <gtk/gtk.h>
#include <gpgme.h>
#include <glib.h>
#include <glib/gi18n.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#ifndef G_OS_WIN32
#  include <sys/wait.h>
#endif
#if (defined(__DragonFly__) || defined(SOLARIS) || defined (__NetBSD__) || defined (__FreeBSD__) || defined (__OpenBSD__))
#  include <sys/signal.h>
#endif
#ifndef G_OS_WIN32
#include <sys/mman.h>
#endif
#if HAVE_LOCALE_H
#  include <locale.h>
#endif

#include "sgpgme.h"
#include "privacy.h"
#include "prefs_common.h"
#include "utils.h"
#include "alertpanel.h"
#include "passphrase.h"
#include "prefs_gpg.h"
#include "account.h"
#include "select-keys.h"

static void sgpgme_disable_all(void)
{
    /* FIXME: set a flag, so that we don't bother the user with failed
     * gpgme messages */
}

gpgme_verify_result_t sgpgme_verify_signature(gpgme_ctx_t ctx, gpgme_data_t sig, 
					gpgme_data_t plain, gpgme_data_t dummy)
{
	gpgme_verify_result_t status = NULL;
	gpgme_error_t err;

	if ((err = gpgme_op_verify(ctx, sig, plain, dummy)) != GPG_ERR_NO_ERROR) {
		debug_print("op_verify err %s\n", gpgme_strerror(err));
		privacy_set_error("%s", gpgme_strerror(err));
		return GINT_TO_POINTER(-GPG_ERR_SYSTEM_ERROR);
	}
	status = gpgme_op_verify_result(ctx);
	if (status && status->signatures == NULL) {
		debug_print("no signature found\n");
		privacy_set_error(_("No signature found"));
		return GINT_TO_POINTER(-GPG_ERR_SYSTEM_ERROR);
	}
	return status;
}

SignatureStatus sgpgme_sigstat_gpgme_to_privacy(gpgme_ctx_t ctx, gpgme_verify_result_t status)
{
	unsigned long validity = 0;
	gpgme_signature_t sig = NULL;
	
	if (GPOINTER_TO_INT(status) == -GPG_ERR_SYSTEM_ERROR) {
		debug_print("system error\n");
		return SIGNATURE_CHECK_FAILED;
	}

	if (status == NULL) {
		debug_print("status == NULL\n");
		return SIGNATURE_UNCHECKED;
	}
	sig = status->signatures;

	if (sig == NULL) {
		debug_print("sig == NULL\n");
		return SIGNATURE_UNCHECKED;
	}
	validity = sig->validity;

	debug_print("err code %d\n", gpg_err_code(sig->status));
	switch (gpg_err_code(sig->status)) {
	case GPG_ERR_NO_ERROR:
		switch (gpg_err_code(sig->validity)) {
		case GPGME_VALIDITY_NEVER:
			return SIGNATURE_INVALID;
		case GPGME_VALIDITY_UNKNOWN:
		case GPGME_VALIDITY_UNDEFINED:
		case GPGME_VALIDITY_MARGINAL:
		case GPGME_VALIDITY_FULL:
		case GPGME_VALIDITY_ULTIMATE:
			return SIGNATURE_OK;
		default:
			return SIGNATURE_CHECK_FAILED;
		}
	case GPG_ERR_SIG_EXPIRED:
	case GPG_ERR_CERT_REVOKED:
		return SIGNATURE_WARN;
	case GPG_ERR_KEY_EXPIRED:
		return SIGNATURE_KEY_EXPIRED;
	case GPG_ERR_BAD_SIGNATURE:
		return SIGNATURE_INVALID;
	case GPG_ERR_NO_PUBKEY:
		return SIGNATURE_CHECK_FAILED;
	default:
		return SIGNATURE_CHECK_FAILED;
	}
	return SIGNATURE_CHECK_FAILED;
}

static const gchar *get_validity_str(unsigned long validity)
{
	switch (gpg_err_code(validity)) {
	case GPGME_VALIDITY_UNKNOWN:
		return _("Unknown");
	case GPGME_VALIDITY_UNDEFINED:
		return _("Undefined");
	case GPGME_VALIDITY_NEVER:
		return _("Never");
	case GPGME_VALIDITY_MARGINAL:
		return _("Marginal");
	case GPGME_VALIDITY_FULL:
		return _("Full");
	case GPGME_VALIDITY_ULTIMATE:
		return _("Ultimate");
	default:
		return _("Error");
	}
}

static gchar *extract_name(const char *uid)
{
	if (uid == NULL)
		return NULL;
	if (!strncmp(uid, "CN=", 3)) {
		gchar *result = g_strdup(uid+3);
		if (strstr(result, ","))
			*(strstr(result, ",")) = '\0';
		return result;
	} else if (strstr(uid, ",CN=")) {
		gchar *result = g_strdup(strstr(uid, ",CN=")+4);
		if (strstr(result, ","))
			*(strstr(result, ",")) = '\0';
		return result;
	} else {
		return g_strdup(uid);
	}
}
gchar *sgpgme_sigstat_info_short(gpgme_ctx_t ctx, gpgme_verify_result_t status)
{
	gpgme_signature_t sig = NULL;
	gchar *uname = NULL;
	gpgme_key_t key;
	gchar *result = NULL;
	gpgme_error_t err = 0;
	static gboolean warned = FALSE;

	if (GPOINTER_TO_INT(status) == -GPG_ERR_SYSTEM_ERROR) {
		return g_strdup_printf(_("The signature can't be checked - %s"), privacy_get_error());
	}

	if (status == NULL) {
		return g_strdup(_("The signature has not been checked."));
	}
	sig = status->signatures;
	if (sig == NULL) {
		return g_strdup(_("The signature has not been checked."));
	}

	err = gpgme_get_key(ctx, sig->fpr, &key, 0);
	if (gpg_err_code(err) == GPG_ERR_NO_AGENT) {
		if (!warned)
			alertpanel_error(_("PGP Core: Can't get key - no gpg-agent running."));
		else
			g_warning(_("PGP Core: Can't get key - no gpg-agent running."));
		warned = TRUE;
	} else if (gpg_err_code(err) != GPG_ERR_NO_ERROR && gpg_err_code(err) != GPG_ERR_EOF) {
		return g_strdup_printf(_("The signature can't be checked - %s"), 
			gpgme_strerror(err));
	}
	if (key)
		uname = extract_name(key->uids->uid);
	else
		uname = g_strdup("<?>");
	switch (gpg_err_code(sig->status)) {
	case GPG_ERR_NO_ERROR:
		switch (gpg_err_code(sig->validity)) {
		case GPGME_VALIDITY_MARGINAL:
		case GPGME_VALIDITY_FULL:
		case GPGME_VALIDITY_ULTIMATE:
			result = g_strdup_printf(_("Good signature from %s."), uname);
			break;
		case GPGME_VALIDITY_UNKNOWN:
		case GPGME_VALIDITY_UNDEFINED:
		case GPGME_VALIDITY_NEVER:
		default:
			result = g_strdup_printf(_("Good signature (untrusted) from %s."), uname);
			break;
		}
		break;
	case GPG_ERR_SIG_EXPIRED:
		result = g_strdup_printf(_("Expired signature from %s."), uname);
		break;
	case GPG_ERR_KEY_EXPIRED:
		result = g_strdup_printf(_("Expired key from %s."), uname);
		break;
	case GPG_ERR_BAD_SIGNATURE:
		result = g_strdup_printf(_("Bad signature from %s."), uname);
		break;
	case GPG_ERR_NO_PUBKEY: {
		gchar *id = g_strdup(sig->fpr + strlen(sig->fpr)-8);
		result = g_strdup_printf(_("Key 0x%s not available to verify this signature."), id);
		g_free(id);
		break;
		}
	default:
		result = g_strdup(_("The signature has not been checked."));
		break;
	}
	if (result == NULL)
		result = g_strdup(_("Error"));
	g_free(uname);
	return result;
}

gchar *sgpgme_sigstat_info_full(gpgme_ctx_t ctx, gpgme_verify_result_t status)
{
	gint i = 0;
	gchar *ret;
	GString *siginfo;
	gpgme_signature_t sig = NULL;

	siginfo = g_string_sized_new(64);
	if (status == NULL) {
		g_string_append_printf(siginfo,
			_("Error checking signature: no status\n"));
		goto bail;
	 }

	sig = status->signatures;
	
	while (sig) {
		gpgme_user_id_t user = NULL;
		gpgme_key_t key;
		gpgme_error_t err;
		const gchar *keytype, *keyid, *uid;
		
		err = gpgme_get_key(ctx, sig->fpr, &key, 0);

		if (err != GPG_ERR_NO_ERROR) {
			key = NULL;
			g_string_append_printf(siginfo, 
				_("Error checking signature: %s\n"),
				gpgme_strerror(err));
			goto bail;
		}
		if (key) {
			user = key->uids;
			keytype = gpgme_pubkey_algo_name(
					key->subkeys->pubkey_algo);
			keyid = key->subkeys->keyid;
			uid = user->uid;
		} else {
			keytype = "?";
			keyid = "?";
			uid = "?";
		}
		g_string_append_printf(siginfo,
			_("Signature made using %s key ID %s\n"),
			keytype, keyid);
		
		switch (gpg_err_code(sig->status)) {
		case GPG_ERR_NO_ERROR:
		case GPG_ERR_KEY_EXPIRED:
			g_string_append_printf(siginfo,
				_("Good signature from uid \"%s\" (Validity: %s)\n"),
				uid, get_validity_str(user?user->validity:GPGME_VALIDITY_UNKNOWN));
			break;
		case GPG_ERR_SIG_EXPIRED:
			g_string_append_printf(siginfo,
				_("Expired signature from uid \"%s\" (Validity: %s)\n"),
				uid, get_validity_str(user?user->validity:GPGME_VALIDITY_UNKNOWN));
			break;
		case GPG_ERR_BAD_SIGNATURE:
			g_string_append_printf(siginfo,
				_("BAD signature from \"%s\"\n"),
				uid);
			break;
		default:
			break;
		}
		if (sig->status != GPG_ERR_BAD_SIGNATURE) {
			gint j = 1;
			user = user ? user->next : NULL;
			while (user != NULL) {
				g_string_append_printf(siginfo,
					_("                    uid \"%s\" (Validity: %s)\n"),
					user->uid,
					get_validity_str(user->validity));
				j++;
				user = user->next;
			}
			g_string_append_printf(siginfo,
				_("Primary key fingerprint:"));
			const char* primary_fpr = NULL;
			if (key && key->subkeys && key->subkeys->fpr)
				primary_fpr = key->subkeys->fpr;
			else
				g_string_append(siginfo, " ?");
			int idx; /* now pretty-print the fingerprint */
			for (idx=0; primary_fpr && *primary_fpr!='\0'; idx++, primary_fpr++) {
				if (idx%4==0)
					g_string_append_c(siginfo, ' ');
				if (idx%20==0)
					g_string_append_c(siginfo, ' ');
				g_string_append_c(siginfo, (gchar)*primary_fpr);
			}
			g_string_append_c(siginfo, '\n');
#ifdef HAVE_GPGME_PKA_TRUST
                        if (sig->pka_trust == 1 && sig->pka_address) {
                                g_string_append_printf(siginfo,
                                   _("WARNING: Signer's address \"%s\" "
                                      "does not match DNS entry\n"), 
                                   sig->pka_address);
                        }
                        else if (sig->pka_trust == 2 && sig->pka_address) {
                                g_string_append_printf(siginfo,
                                   _("Verified signer's address is \"%s\"\n"),
                                   sig->pka_address);
                                /* FIXME: Compare the address to the
                                 * From: address.  */
                        }
#endif /*HAVE_GPGME_PKA_TRUST*/
		}

		g_string_append(siginfo, "\n");
		i++;
		sig = sig->next;
	}
bail:
	ret = siginfo->str;
	g_string_free(siginfo, FALSE);
	return ret;
}

gpgme_data_t sgpgme_data_from_mimeinfo(MimeInfo *mimeinfo)
{
	gpgme_data_t data = NULL;
	gpgme_error_t err;
	FILE *fp = g_fopen(mimeinfo->data.filename, "rb");
	gchar *tmp_file = NULL;

	if (!fp) 
		return NULL;

	tmp_file = get_tmp_file();
	copy_file_part(fp, mimeinfo->offset, mimeinfo->length, tmp_file);
	fclose(fp);
	fp = NULL;
	debug_print("tmp file %s\n", tmp_file);
	
	err = gpgme_data_new_from_file(&data, tmp_file, 1);
	claws_unlink(tmp_file);
	g_free(tmp_file);

	debug_print("data %p (%d %d)\n", (void *)&data, mimeinfo->offset, mimeinfo->length);
	if (err) {
		debug_print ("gpgme_data_new_from_file failed: %s\n",
			     gpgme_strerror (err));
		privacy_set_error(_("Couldn't get data from message, %s"), gpgme_strerror(err));
		return NULL;
	}
	return data;
}

gpgme_data_t sgpgme_decrypt_verify(gpgme_data_t cipher, gpgme_verify_result_t *status, gpgme_ctx_t ctx)
{
	struct passphrase_cb_info_s info;
	gpgme_data_t plain;
	gpgme_error_t err;

	memset (&info, 0, sizeof info);
	
	if ((err = gpgme_data_new(&plain)) != GPG_ERR_NO_ERROR) {
		gpgme_release(ctx);
		privacy_set_error(_("Couldn't initialize data, %s"), gpgme_strerror(err));
		return NULL;
	}
	
	if (gpgme_get_protocol(ctx) == GPGME_PROTOCOL_OpenPGP) {
		prefs_gpg_enable_agent(prefs_gpg_get_config()->use_gpg_agent);
    		if (!getenv("GPG_AGENT_INFO") || !prefs_gpg_get_config()->use_gpg_agent) {
        		info.c = ctx;
        		gpgme_set_passphrase_cb (ctx, gpgmegtk_passphrase_cb, &info);
    		}
	} else {
		prefs_gpg_enable_agent(TRUE);
        	info.c = ctx;
        	gpgme_set_passphrase_cb (ctx, NULL, &info);
	}
	
	
	if (gpgme_get_protocol(ctx) == GPGME_PROTOCOL_OpenPGP) {
		err = gpgme_op_decrypt_verify(ctx, cipher, plain);
		if (err != GPG_ERR_NO_ERROR) {
			debug_print("can't decrypt (%s)\n", gpgme_strerror(err));
			privacy_set_error("%s", gpgme_strerror(err));
			gpgmegtk_free_passphrase();
			gpgme_data_release(plain);
			return NULL;
		}

		err = cm_gpgme_data_rewind(plain);
		if (err) {
			debug_print("can't seek (%d %d %s)\n", err, errno, strerror(errno));
		}

		debug_print("decrypted.\n");
		*status = gpgme_op_verify_result (ctx);
	} else {
		err = gpgme_op_decrypt(ctx, cipher, plain);
		if (err != GPG_ERR_NO_ERROR) {
			debug_print("can't decrypt (%s)\n", gpgme_strerror(err));
			privacy_set_error("%s", gpgme_strerror(err));
			gpgmegtk_free_passphrase();
			gpgme_data_release(plain);
			return NULL;
		}

		err = cm_gpgme_data_rewind(plain);
		if (err) {
			debug_print("can't seek (%d %d %s)\n", err, errno, strerror(errno));
		}

		debug_print("decrypted.\n");
		*status = gpgme_op_verify_result (ctx);
	}
	return plain;
}

gchar *sgpgme_get_encrypt_data(GSList *recp_names, gpgme_protocol_t proto)
{
	SelectionResult result = KEY_SELECTION_CANCEL;
	gpgme_key_t *keys = gpgmegtk_recipient_selection(recp_names, &result,
				proto);
	gchar *ret = NULL;
	int i = 0;

	if (!keys) {
		if (result == KEY_SELECTION_DONT)
			return g_strdup("_DONT_ENCRYPT_");
		else
			return NULL;
	}
	while (keys[i]) {
		gpgme_subkey_t skey = keys[i]->subkeys;
		gchar *fpr = skey->fpr;
		gchar *tmp = NULL;
		debug_print("adding %s\n", fpr);
		tmp = g_strconcat(ret?ret:"", fpr, " ", NULL);
		g_free(ret);
		ret = tmp;
		i++;
	}
	return ret;
}

gboolean sgpgme_setup_signers(gpgme_ctx_t ctx, PrefsAccount *account,
			      const gchar *from_addr)
{
	GPGAccountConfig *config;
	const gchar *signer_addr = account->address;

	gpgme_signers_clear(ctx);

	if (from_addr)
		signer_addr = from_addr;
	config = prefs_gpg_account_get_config(account);

	switch(config->sign_key) {
	case SIGN_KEY_DEFAULT:
		debug_print("using default gnupg key\n");
		break;
	case SIGN_KEY_BY_FROM:
		debug_print("using key for %s\n", signer_addr);
		break;
	case SIGN_KEY_CUSTOM:
		debug_print("using key for %s\n", config->sign_key_id);
		break;
	}

	if (config->sign_key != SIGN_KEY_DEFAULT) {
		const gchar *keyid;
		gpgme_key_t key, key2;
		gpgme_error_t err;

		if (config->sign_key == SIGN_KEY_BY_FROM)
			keyid = signer_addr;
		else if (config->sign_key == SIGN_KEY_CUSTOM)
			keyid = config->sign_key_id;
		else
			goto bail;

		err = gpgme_op_keylist_start(ctx, keyid, 1);
		if (!err) {
			do {
				err = gpgme_op_keylist_next(ctx, &key);
				if (!err && key && key->protocol == gpgme_get_protocol(ctx) &&
				    !key->expired && !key->revoked && !key->disabled)
					break;
				if (!err && key && key->protocol != gpgme_get_protocol(ctx)) {
					debug_print("skipping a key (wrong protocol %d)\n", key->protocol);
					gpgme_key_release(key);
				}
				if (!err && key && (key->expired || key->revoked || key->disabled)) {
					
					debug_print("skipping a key");
					if (key->expired) 
						debug_print(" expired");
					if (key->revoked) 
						debug_print(" revoked");
					if (key->disabled) 
						debug_print(" disabled");
					debug_print("\n");
					gpgme_key_release(key);
				}
			} while (!err);
		}
		if (err) {
			g_warning("setup_signers start: %s", gpgme_strerror(err));
			privacy_set_error(_("Secret key not found (%s)"), gpgme_strerror(err));
			goto bail;
		}
		
		do {
			err = gpgme_op_keylist_next(ctx, &key2);
			if (!err && key2 && key2->protocol == gpgme_get_protocol(ctx) &&
			    !key2->expired && !key2->revoked && !key2->disabled)
				break;
			if (!err && key2 && key2->protocol != gpgme_get_protocol(ctx)) {
				debug_print("skipping a key (wrong protocol %d)\n", key2->protocol);
				gpgme_key_release(key2);
			}
			if (!err && key2 && (key2->expired || key2->revoked || key2->disabled)) {
					debug_print("skipping a key");
					if (key2->expired) 
						debug_print(" expired");
					if (key2->revoked) 
						debug_print(" revoked");
					if (key2->disabled) 
						debug_print(" disabled");
					debug_print("\n");
				gpgme_key_release(key2);
			}
		} while (!err);
		if (!err) {
			gpgme_key_release(key2);
			g_warning("ambiguous specification of secret key '%s'\n",
				keyid);
			privacy_set_error(_("Secret key specification is ambiguous"));
			goto bail;
		}
		
		gpgme_op_keylist_end(ctx);
		err = gpgme_signers_add(ctx, key);
		debug_print("got key (proto %d (pgp %d, smime %d).\n", key->protocol,
				GPGME_PROTOCOL_OpenPGP, GPGME_PROTOCOL_CMS);
		gpgme_key_release(key);
		
		if (err) {
			g_warning("error adding secret key: %s\n", gpgme_strerror(err));
			privacy_set_error(_("Error setting secret key: %s"), gpgme_strerror(err));
			goto bail;
		}
	}

	prefs_gpg_account_free_config(config);

	return TRUE;
bail:
	prefs_gpg_account_free_config(config);
	return FALSE;
}

void sgpgme_init()
{
	gpgme_engine_info_t engineInfo;
	if (gpgme_check_version("1.0.0")) {
#ifdef LC_CTYPE
		gpgme_set_locale(NULL, LC_CTYPE, setlocale(LC_CTYPE, NULL));
#endif
#ifdef LC_MESSAGES
		gpgme_set_locale(NULL, LC_MESSAGES, setlocale(LC_MESSAGES, NULL));
#endif
		if (!gpgme_get_engine_info(&engineInfo)) {
			while (engineInfo) {
#ifndef G_OS_WIN32
				debug_print("GpgME Protocol: %s\n"
					    "Version: %s (req %s)\n"
					    "Executable: %s\n",
					gpgme_get_protocol_name(engineInfo->protocol) ? gpgme_get_protocol_name(engineInfo->protocol):"???",
					engineInfo->version ? engineInfo->version:"???",
					engineInfo->req_version ? engineInfo->req_version:"???",
					engineInfo->file_name ? engineInfo->file_name:"???");
#endif
				if (engineInfo->protocol == GPGME_PROTOCOL_OpenPGP
				&&  gpgme_engine_check_version(engineInfo->protocol) != 
					GPG_ERR_NO_ERROR) {
					if (engineInfo->file_name && !engineInfo->version) {
						alertpanel_error(_("Gpgme protocol '%s' is unusable: "
								   "Engine '%s' isn't installed properly."),
								   gpgme_get_protocol_name(engineInfo->protocol),
								   engineInfo->file_name);
					} else if (engineInfo->file_name && engineInfo->version
					  && engineInfo->req_version) {
						alertpanel_error(_("Gpgme protocol '%s' is unusable: "
								   "Engine '%s' version %s is installed, "
								   "but version %s is required.\n"),
								   gpgme_get_protocol_name(engineInfo->protocol),
								   engineInfo->file_name,
								   engineInfo->version,
								   engineInfo->req_version);
					} else {
						alertpanel_error(_("Gpgme protocol '%s' is unusable "
								   "(unknown problem)"),
								   gpgme_get_protocol_name(engineInfo->protocol));
					}
				}
				engineInfo = engineInfo->next;
			}
		}
	} else {
		sgpgme_disable_all();

		if (prefs_gpg_get_config()->gpg_warning) {
			AlertValue val;

			val = alertpanel_full
				(_("Warning"),
				 _("GnuPG is not installed properly, or needs "
				 "to be upgraded.\n"
				 "OpenPGP support disabled."),
				 GTK_STOCK_CLOSE, NULL, NULL, TRUE, NULL,
				 ALERT_WARNING, G_ALERTDEFAULT);
			if (val & G_ALERTDISABLE)
				prefs_gpg_get_config()->gpg_warning = FALSE;
		}
	}
}

void sgpgme_done()
{
        gpgmegtk_free_passphrase();
}

void sgpgme_create_secret_key(PrefsAccount *account, gboolean ask_create)
{
	AlertValue val = G_ALERTDEFAULT;
	gchar *key_parms = NULL;
	gchar *name = NULL;
	gchar *email = NULL;
	gchar *passphrase = NULL, *passphrase_second = NULL;
	gint prev_bad = 0;
	gchar *tmp = NULL;
	gpgme_error_t err = 0;
	gpgme_ctx_t ctx;
	GtkWidget *window = NULL;
	gpgme_genkey_result_t key;

	if (account == NULL)
		account = account_get_default();

	if (account->address == NULL) {
		alertpanel_error(_("You have to save the account's information with \"OK\" "
				   "before being able to generate a key pair.\n"));
		return;
	}
	if (ask_create) {
		val = alertpanel(_("No PGP key found"),
				_("Claws Mail did not find a secret PGP key, "
				  "which means that you won't be able to sign "
				  "emails or receive encrypted emails.\n"
				  "Do you want to create a new key pair now?"),
				  GTK_STOCK_NO, "+" GTK_STOCK_YES, NULL);
		if (val == G_ALERTDEFAULT) {
			prefs_gpg_get_config()->gpg_ask_create_key = FALSE;
			prefs_gpg_save_config();
			return;
		}
	}

	if (account->name) {
		name = g_strdup(account->name);
	} else {
		name = g_strdup(account->address);
	}
	email = g_strdup(account->address);
	tmp = g_strdup_printf("%s <%s>", account->name?account->name:account->address, account->address);
again:
	passphrase = passphrase_mbox(tmp, NULL, prev_bad, 1);
	if (passphrase == NULL) {
		g_free(tmp);
		g_free(email);
		g_free(name);		
		return;
	}
	passphrase_second = passphrase_mbox(tmp, NULL, 0, 2);
	if (passphrase_second == NULL) {
		g_free(tmp);
		g_free(email);
		g_free(passphrase);		
		g_free(name);		
		return;
	}
	if (strcmp(passphrase, passphrase_second)) {
		g_free(passphrase);
		g_free(passphrase_second);
		prev_bad = 1;
		goto again;
	}
	
	key_parms = g_strdup_printf("<GnupgKeyParms format=\"internal\">\n"
					"Key-Type: DSA\n"
					"Key-Length: 1024\n"
					"Subkey-Type: ELG-E\n"
					"Subkey-Length: 2048\n"
					"Name-Real: %s\n"
					"Name-Email: %s\n"
					"Expire-Date: 0\n"
					"%s%s%s"
					"</GnupgKeyParms>\n",
					name, email, 
					strlen(passphrase)?"Passphrase: ":"",
					passphrase,
					strlen(passphrase)?"\n":"");
#ifndef G_PLATFORM_WIN32
	if (mlock(passphrase, strlen(passphrase)) == -1)
		debug_print("couldn't lock passphrase\n");
	if (mlock(passphrase_second, strlen(passphrase_second)) == -1)
		debug_print("couldn't lock passphrase2\n");
#endif
	g_free(tmp);
	g_free(email);
	g_free(name);
	g_free(passphrase_second);
	g_free(passphrase);
	
	err = gpgme_new (&ctx);
	if (err) {
		alertpanel_error(_("Couldn't generate a new key pair: %s"),
				 gpgme_strerror(err));
		g_free(key_parms);
		return;
	}
	

	window = label_window_create(_("Generating your new key pair... Please move the mouse "
			      "around to help generate entropy..."));

	err = gpgme_op_genkey(ctx, key_parms, NULL, NULL);
	g_free(key_parms);

	label_window_destroy(window);

	if (err) {
		alertpanel_error(_("Couldn't generate a new key pair: %s"), gpgme_strerror(err));
		gpgme_release(ctx);
		return;
	}
	key = gpgme_op_genkey_result(ctx);
	if (key == NULL) {
		alertpanel_error(_("Couldn't generate a new key pair: unknown error"));
		gpgme_release(ctx);
		return;
	} else {
		gchar *buf = g_strdup_printf(_("Your new key pair has been generated. "
				    "Its fingerprint is:\n%s\n\nDo you want to export it "
				    "to a keyserver?"),
				    key->fpr ? key->fpr:"null");
		AlertValue val = alertpanel(_("Key generated"), buf,
				  GTK_STOCK_NO, "+" GTK_STOCK_YES, NULL);
		g_free(buf);
		if (val == G_ALERTALTERNATE) {
#ifndef G_OS_WIN32
			gchar *cmd = g_strdup_printf("gpg --no-tty --send-keys %s", key->fpr);
			int res = 0;
			pid_t pid = 0;
			pid = fork();
			if (pid == -1) {
				res = -1;
			} else if (pid == 0) {
				/* son */
				res = system(cmd);
				res = WEXITSTATUS(res);
				_exit(res);
			} else {
				int status = 0;
				time_t start_wait = time(NULL);
				res = -1;
				do {
					if (waitpid(pid, &status, WNOHANG) == 0 || !WIFEXITED(status)) {
						usleep(200000);
					} else {
						res = WEXITSTATUS(status);
						break;
					}
					if (time(NULL) - start_wait > 5) {
						debug_print("SIGTERM'ing gpg\n");
						kill(pid, SIGTERM);
					}
					if (time(NULL) - start_wait > 6) {
						debug_print("SIGKILL'ing gpg\n");
						kill(pid, SIGKILL);
						break;
					}
				} while(1);
			}
			if (res == 0) {
				alertpanel_notice(_("Key exported."));
			} else {
				alertpanel_error(_("Couldn't export key."));
			}
			g_free(cmd);
#else
			alertpanel_error(_("Key export isn't implemented in Windows."));
#endif
		}
	}
	prefs_gpg_get_config()->gpg_ask_create_key = FALSE;
	prefs_gpg_save_config();
	gpgme_release(ctx);
}

gboolean sgpgme_has_secret_key(void)
{
	gpgme_error_t err = 0;
	gpgme_ctx_t ctx;
	gpgme_key_t key;

	err = gpgme_new (&ctx);
	if (err) {
		debug_print("err : %s\n", gpgme_strerror(err));
		return TRUE;
	}
check_again:
	err = gpgme_op_keylist_start(ctx, NULL, TRUE);
	if (!err)
		err = gpgme_op_keylist_next(ctx, &key);
	gpgme_op_keylist_end(ctx);
	if (gpg_err_code(err) == GPG_ERR_EOF) {
		if (gpgme_get_protocol(ctx) != GPGME_PROTOCOL_CMS) {
			gpgme_set_protocol(ctx, GPGME_PROTOCOL_CMS);
			goto check_again;
		}
		gpgme_release(ctx);
		return FALSE;
	} else {
		gpgme_release(ctx);
		return TRUE;
	}
}

void sgpgme_check_create_key(void)
{
	if (prefs_gpg_get_config()->gpg_ask_create_key &&
	    !sgpgme_has_secret_key()) {
		sgpgme_create_secret_key(NULL, TRUE);
	} else {
		prefs_gpg_get_config()->gpg_ask_create_key = FALSE;
		prefs_gpg_save_config();
	}	
}

void *sgpgme_data_release_and_get_mem(gpgme_data_t data, size_t *len)
{
	char buf[BUFSIZ];
	void *result = NULL;
	ssize_t r = 0;
	size_t w = 0;
	
	if (data == NULL)
		return NULL;
	if (len == NULL)
		return NULL;

	/* I know it's deprecated, but we don't compile with _LARGEFILE */
	cm_gpgme_data_rewind(data);
	while ((r = gpgme_data_read(data, buf, BUFSIZ)) > 0) {
		result = realloc(result, r + w);
		memcpy(result+w, buf, r);
		w += r;
	}
	
	*len = w;

	gpgme_data_release(data);
	if (r < 0) {
		free(result);
		*len = 0;
		return NULL;
	}
	return result;
}

gpgme_error_t cm_gpgme_data_rewind(gpgme_data_t dh)
{
#if defined(_FILE_OFFSET_BITS) && _FILE_OFFSET_BITS == 64
	if (gpgme_data_seek(dh, (off_t)0, SEEK_SET) == -1)
		return gpg_error_from_errno(errno);
	else
		return 0;
#else
	return gpgme_data_rewind(dh);
#endif
}

#endif /* USE_GPGME */
