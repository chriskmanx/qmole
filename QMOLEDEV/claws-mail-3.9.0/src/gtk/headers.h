/* all header names, with or without trailing colon */
static struct {
	const gchar *header_name;
	const gchar *header_name_w_colon;
} HEADERS[] = {

	/* RFC2822 */
	{ N_("Date"),						  N_("Date:") },
	{ N_("From"),						  N_("From:") },
	{ N_("Sender"), 					  N_("Sender:") },
	{ N_("Reply-To"),					  N_("Reply-To:") },
	{ N_("To"), 						  N_("To:") },
	{ N_("Cc"), 						  N_("Cc:") },
	{ N_("Bcc"),						  N_("Bcc:") },
	{ N_("Message-ID"), 				  N_("Message-ID:") },
	{ N_("In-Reply-To"),				  N_("In-Reply-To:") },
	{ N_("References"), 				  N_("References:") },
	{ N_("Subject"),					  N_("Subject:") },
	{ N_("Comments"),					  N_("Comments:") },
	{ N_("Keywords"),					  N_("Keywords:") },
	{ N_("Resent-Date"),				  N_("Resent-Date:") },
	{ N_("Resent-From"),				  N_("Resent-From:") },
	{ N_("Resent-Sender"),				  N_("Resent-Sender:") },
	{ N_("Resent-To"),					  N_("Resent-To:") },
	{ N_("Resent-Cc"),					  N_("Resent-Cc:") },
	{ N_("Resent-Bcc"), 				  N_("Resent-Bcc:") },
	{ N_("Resent-Message-ID"),			  N_("Resent-Message-ID:") },
	{ N_("Return-Path"),				  N_("Return-Path:") },
	{ N_("Received"),					  N_("Received:") },

	/* more */
	{ N_("Newsgroups"), 				  N_("Newsgroups:") },
	{ N_("Followup-To"),				  N_("Followup-To:") },
	{ N_("Delivered-To"),				  N_("Delivered-To:") },
	{ N_("Seen"),						  N_("Seen:") },
	{ N_("Status"), 					  N_("Status:") },
	{ N_("Face"),						  N_("Face:") },
	{ N_("Disposition-Notification-To"),  N_("Disposition-Notification-To:") },
	{ N_("Return-Receipt-To"),			  N_("Return-Receipt-To:") },
	{ N_("User-Agent"), 				  N_("User-Agent:") },
	{ N_("Content-Type"),				  N_("Content-Type:") },
	{ N_("Content-Transfer-Encoding"),    N_("Content-Transfer-Encoding:") },
	{ N_("MIME-Version"),				  N_("MIME-Version:") },
	{ N_("Precedence"), 				  N_("Precedence:") },
	{ N_("Organization"),				  N_("Organization:") },

	{ N_("Mailing-List"),				  N_("Mailing-List:") },
	{ N_("List-Post"),					  N_("List-Post:") },
	{ N_("List-Subscribe"), 			  N_("List-Subscribe:") },
	{ N_("List-Unsubscribe"),			  N_("List-Unsubscribe:") },
	{ N_("List-Help"),					  N_("List-Help:") },
	{ N_("List-Archive"),				  N_("List-Archive:") },
	{ N_("List-Owner"), 				  N_("List-Owner:") },

	{ N_("X-Label"),					  N_("X-Label:") },
	{ N_("X-Mailer"),					  N_("X-Mailer:") },
	{ N_("X-Status"),					  N_("X-Status:") },
	{ N_("X-Face"), 					  N_("X-Face:") },
	{ N_("X-No-Archive"),				  N_("X-No-Archive:") },

/* some common logical names referring to real header names */
	{ N_("In reply to"),				  N_("In reply to:") },
	{ N_("To or Cc"),					  N_("To or Cc:") },
	{ N_("From, To or Subject"),		  N_("From, To or Subject:") },
};
