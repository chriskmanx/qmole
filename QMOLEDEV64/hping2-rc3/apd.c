/* Copyright (C) 2000,2001 Salvatore Sanfilippo <antirez@invece.org>
 * See the LICENSE file for more information.
 *
 * ARS Packet Description System.
 *
 * Please, prefix all the function with ars_d_ */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "ars.h"

#define ARS_MAX_TSIZE	1024
char *ars_d_parser(char *t, char *next, size_t size)
{
	int i = 0;

	if (size == 0 || next == NULL || *t == '\0')
		return NULL;
	size--; /* space for nul term */
	while (1) {
		/* no space for the next char */
		if (i == size) {
			next[i] = '\0';
			return t;
		}
		switch(*t) {
		case '\0':
		case '{':
		case '}':
		case ',':
		case '=':
		case '+':
			if (i == 0) {
				next[i] = *t;
				next[i+1] = '\0';
				return t+1;
			} else {
				next[i] = '\0';
				return t;
			}
		default:
			next[i++] = *t++;
			break;
		}
	}
	return NULL; /* unreached */
}

/* states */
#define ARS_G_LAYER		0
#define ARS_G_FIELD		1
#define ARS_G_VALUE		2
#define ARS_G_OBRACE_OR_PLUS	3
#define ARS_G_CBRACE		4
#define ARS_G_COMMA_OR_CBRACE	5
#define ARS_G_LEN_OR_PLUS	6
#define ARS_G_PLUS		7
#define ARS_G_EQUAL		8

struct ars_d_keyword_info {
	char *ki_keyword;
	int ki_opt;
	void *(*ki_add) (struct ars_packet *pkt, int opt);
	int (*ki_set) (struct ars_packet *pkt, int layer, char *f, char *v);
};

#define ARS_DKINFO_SIZE		64

#define BOGUS_SET_F(x) \
  int (x)(struct ars_packet *pkt, int layer, char *f, char *v) { return 0; }

int ars_d_set_ip(struct ars_packet *pkt, int layer, char *f, char *v);
int ars_d_set_udp(struct ars_packet *pkt, int layer, char *f, char *v);
int ars_d_set_tcp(struct ars_packet *pkt, int layer, char *f, char *v);
int ars_d_set_icmp(struct ars_packet *pkt, int layer, char *f, char *v);
int ars_d_set_data(struct ars_packet *pkt, int layer, char *f, char *v);
BOGUS_SET_F(ars_d_set_ipopt_sec)
BOGUS_SET_F(ars_d_set_ipopt_sid)
BOGUS_SET_F(ars_d_set_ipopt_lsrr)
BOGUS_SET_F(ars_d_set_ipopt_ssrr)
BOGUS_SET_F(ars_d_set_ipopt_rr)
BOGUS_SET_F(ars_d_set_ipopt_ts)
BOGUS_SET_F(ars_d_set_tcpopt_mss)
BOGUS_SET_F(ars_d_set_tcpopt_wscale)
BOGUS_SET_F(ars_d_set_tcpopt_sackperm)
BOGUS_SET_F(ars_d_set_tcpopt_sack)
BOGUS_SET_F(ars_d_set_tcpopt_echo)
BOGUS_SET_F(ars_d_set_tcpopt_echoreply)
BOGUS_SET_F(ars_d_set_tcpopt_ts)

struct ars_d_keyword_info ars_dkinfo[ARS_DKINFO_SIZE] = {
	/* KEYWORD	OPT		ADD function	SET function *
	 * --------------------------------------------------------- */
	{"ip",		0,		ars_add_iphdr,	ars_d_set_ip},
	{"ipopt.eol",	ARS_IPOPT_EOL,	ars_add_ipopt,	NULL},
	{"ipopt.nop",	ARS_IPOPT_NOP,	ars_add_ipopt,	NULL},
	{"ipopt.sec",	ARS_IPOPT_SEC,	ars_add_ipopt,	ars_d_set_ipopt_sec},
	{"ipopt.sid",	ARS_IPOPT_SID,	ars_add_ipopt,	ars_d_set_ipopt_sid},
	{"ipopt.lsrr",	ARS_IPOPT_LSRR,	ars_add_ipopt,	ars_d_set_ipopt_lsrr},
	{"ipopt.ssrr",	ARS_IPOPT_SSRR,	ars_add_ipopt,	ars_d_set_ipopt_ssrr},
	{"ipopt.rr",	ARS_IPOPT_RR,	ars_add_ipopt,	ars_d_set_ipopt_rr},
	{"ipopt.ts",	ARS_IPOPT_TIMESTAMP, ars_add_ipopt, ars_d_set_ipopt_ts},
	{"udp",		0,		ars_add_udphdr,	ars_d_set_udp},
	{"tcp",		0,		ars_add_tcphdr,	ars_d_set_tcp},
	{"tcpopt.end",	ARS_TCPOPT_EOL,	ars_add_tcpopt,	NULL},
	{"tcpopt.nop",	ARS_TCPOPT_NOP,	ars_add_tcpopt,	NULL},
	{"tcpopt.mss",	ARS_TCPOPT_MAXSEG, ars_add_tcpopt, ars_d_set_tcpopt_mss},
	{"tcpopt.wscale", ARS_TCPOPT_WINDOW, ars_add_tcpopt, ars_d_set_tcpopt_wscale},
	{"tcpopt.sackperm", ARS_TCPOPT_SACK_PERM, ars_add_tcpopt, ars_d_set_tcpopt_sackperm},
	{"tcpopt.sack", ARS_TCPOPT_SACK, ars_add_tcpopt, ars_d_set_tcpopt_sack},
	{"tcpopt.echo", ARS_TCPOPT_ECHOREQUEST, ars_add_tcpopt, ars_d_set_tcpopt_echo},
	{"tcpopt.echoreply", ARS_TCPOPT_ECHOREPLY, ars_add_tcpopt, ars_d_set_tcpopt_echoreply},
	{"tcpopt.ts",	ARS_TCPOPT_TIMESTAMP, ars_add_tcpopt, ars_d_set_tcpopt_ts},
	{"icmp",	0,		ars_add_icmphdr, ars_d_set_icmp},
	{"data",	0,		ars_add_data,	ars_d_set_data},
	{NULL, 0, NULL, NULL} /* nul term */
};

struct ars_d_keyword_info *ars_get_keyword_by_name(char *name)
{
	struct ars_d_keyword_info *k = ars_dkinfo;

	while (k->ki_keyword) {
		if (strcasecmp(k->ki_keyword, name) == 0)
			return k;
		k++;
	}
	return NULL;
}

int ars_d_setlayer_size(struct ars_packet *pkt, int layer, char *size)
{
	size_t newsize;

	if (layer == ARS_LAST_LAYER)
		layer = pkt->p_layer_nr - 1;
	if (ars_valid_layer(layer) != -ARS_OK)
		return -ARS_INVALID;

	newsize = ars_atou(size);
	if (newsize < 1 || newsize > pkt->p_layer[layer].l_size) {
		ars_set_error(pkt, "Invalid layer size in description");
		return -ARS_INVALID;
	}
	pkt->p_layer[layer].l_size = newsize;

	__D(printf("Setting the layer to size %s\n", size);)
	return -ARS_OK;
}

int ars_d_set_ip(struct ars_packet *pkt, int layer, char *f, char *v)
{
	struct ars_iphdr *ip;

	if (layer == ARS_LAST_LAYER)
		layer = pkt->p_layer_nr - 1;
	if (ars_valid_layer(layer) != -ARS_OK)
		return -ARS_INVALID;

	ip = pkt->p_layer[layer].l_data;

	if (strcasecmp(f, "saddr") == 0) {
		return ars_resolve(pkt, &ip->saddr, v);
	} else if (strcasecmp(f, "daddr") == 0) {
		return ars_resolve(pkt, &ip->daddr, v);
	} else if (strcasecmp(f, "ihl") == 0) {
		ip->ihl = ars_atou(v);
		pkt->p_layer[layer].l_flags |= ARS_TAKE_IP_HDRLEN;
	} else if (strcasecmp(f, "ver") == 0) {
		ip->version = ars_atou(v);
		pkt->p_layer[layer].l_flags |= ARS_TAKE_IP_VERSION;
	} else if (strcasecmp(f, "tos") == 0) {
		ip->tos = ars_atou(v);
	} else if (strcasecmp(f, "totlen") == 0) {
		ip->tot_len = htons(ars_atou(v));
		pkt->p_layer[layer].l_flags |= ARS_TAKE_IP_TOTLEN;
	} else if (strcasecmp(f, "id") == 0) {
		ip->id = htons(ars_atou(v));
	} else if (strcasecmp(f, "fragoff") == 0) {
		ip->frag_off = ip->frag_off & 0xE000;
		ip->frag_off = htons(ars_atou(v) >> 3);
	} else if (strcasecmp(f, "mf") == 0) {
		if (ars_atou(v) == 0)
			ip->frag_off &= htons(~ARS_IP_MF);
		else
			ip->frag_off |= htons(ARS_IP_MF);
	} else if (strcasecmp(f, "df") == 0) {
		if (ars_atou(v) == 0)
			ip->frag_off &= htons(~ARS_IP_DF);
		else
			ip->frag_off |= htons(ARS_IP_DF);
	} else if (strcasecmp(f, "rf") == 0) {
		if (ars_atou(v) == 0)
			ip->frag_off &= htons((u_int16_t)~ARS_IP_RF);
		else
			ip->frag_off |= htons(ARS_IP_RF);
	} else if (strcasecmp(f, "ttl") == 0) {
		ip->ttl = ars_atou(v);
	} else if (strcasecmp(f, "proto") == 0) {
		ip->protocol = ars_atou(v);
		pkt->p_layer[layer].l_flags |= ARS_TAKE_IP_PROTOCOL;
	} else if (strcasecmp(f, "cksum") == 0) {
		ip->check = htons(ars_atou(v));
		pkt->p_layer[layer].l_flags |= ARS_TAKE_IP_CKSUM;
	} else {
		ars_set_error(pkt, "Invalid field for IP layer");
		return -ARS_INVALID;
	}
	return -ARS_OK;
}

int ars_d_set_udp(struct ars_packet *pkt, int layer, char *f, char *v)
{
	struct ars_udphdr *udp;

	if (layer == ARS_LAST_LAYER)
		layer = pkt->p_layer_nr - 1;
	if (ars_valid_layer(layer) != -ARS_OK)
		return -ARS_INVALID;

	udp = pkt->p_layer[layer].l_data;

	if (strcasecmp(f, "sport") == 0) {
		udp->uh_sport = htons(ars_atou(v));
	} else if (strcasecmp(f, "dport") == 0) {
		udp->uh_dport = htons(ars_atou(v));
	} else if (strcasecmp(f, "len") == 0) {
		udp->uh_ulen = htons(ars_atou(v));
		pkt->p_layer[layer].l_flags |= ARS_TAKE_UDP_LEN;
	} else if (strcasecmp(f, "cksum") == 0) {
		udp->uh_sum = htons(ars_atou(v));
		pkt->p_layer[layer].l_flags |= ARS_TAKE_UDP_CKSUM;
	} else {
		ars_set_error(pkt, "Invalid field for UDP layer");
		return -ARS_INVALID;
	}
	return -ARS_OK;
}

int ars_d_set_tcp(struct ars_packet *pkt, int layer, char *f, char *v)
{
	struct ars_tcphdr *tcp;

	if (layer == ARS_LAST_LAYER)
		layer = pkt->p_layer_nr - 1;
	if (ars_valid_layer(layer) != -ARS_OK)
		return -ARS_INVALID;

	tcp = pkt->p_layer[layer].l_data;

	if (strcasecmp(f, "sport") == 0) {
		tcp->th_sport = htons(ars_atou(v));
	} else if (strcasecmp(f, "dport") == 0) {
		tcp->th_dport = htons(ars_atou(v));
	} else if (strcasecmp(f, "seq") == 0) {
		tcp->th_seq = htonl(ars_atou(v));
	} else if (strcasecmp(f, "ack") == 0) {
		tcp->th_ack = htonl(ars_atou(v));
	} else if (strcasecmp(f, "x2") == 0) {
		tcp->th_x2 = ars_atou(v);
	} else if (strcasecmp(f, "off") == 0) {
		tcp->th_off = ars_atou(v);
		pkt->p_layer[layer].l_flags |= ARS_TAKE_TCP_HDRLEN;
	} else if (strcasecmp(f, "flags") == 0) {
		tcp->th_flags = 0;
		if (strchr(v, 'f') || strchr(v, 'F'))
			tcp->th_flags |= ARS_TCP_TH_FIN;
		if (strchr(v, 's') || strchr(v, 'S'))
			tcp->th_flags |= ARS_TCP_TH_SYN;
		if (strchr(v, 'r') || strchr(v, 'R'))
			tcp->th_flags |= ARS_TCP_TH_RST;
		if (strchr(v, 'p') || strchr(v, 'P'))
			tcp->th_flags |= ARS_TCP_TH_PUSH;
		if (strchr(v, 'a') || strchr(v, 'A'))
			tcp->th_flags |= ARS_TCP_TH_ACK;
		if (strchr(v, 'u') || strchr(v, 'U'))
			tcp->th_flags |= ARS_TCP_TH_URG;
		if (strchr(v, 'x') || strchr(v, 'X'))
			tcp->th_flags |= ARS_TCP_TH_X;
		if (strchr(v, 'y') || strchr(v, 'Y'))
			tcp->th_flags |= ARS_TCP_TH_Y;
	} else if (strcasecmp(f, "win") == 0) {
		tcp->th_win = htons(ars_atou(v));
	} else if (strcasecmp(f, "cksum") == 0) {
		tcp->th_sum = htons(ars_atou(v));
		pkt->p_layer[layer].l_flags |= ARS_TAKE_TCP_CKSUM;
	} else if (strcasecmp(f, "urp") == 0) {
		tcp->th_urp = htons(ars_atou(v));
	} else {
		ars_set_error(pkt, "Invalid field for TCP layer");
		return -ARS_INVALID;
	}
	return -ARS_OK;
}

int ars_d_set_icmp(struct ars_packet *pkt, int layer, char *f, char *v)
{
	struct ars_icmphdr *icmp;

	if (layer == ARS_LAST_LAYER)
		layer = pkt->p_layer_nr - 1;
	if (ars_valid_layer(layer) != -ARS_OK)
		return -ARS_INVALID;

	icmp = pkt->p_layer[layer].l_data;

	if (strcasecmp(f, "type") == 0) {
		icmp->type = ars_atou(v);
	} else if (strcasecmp(f, "code") == 0) {
		icmp->code = ars_atou(v);
	} else if (strcasecmp(f, "cksum") == 0) {
		icmp->checksum = htons(ars_atou(v));
		pkt->p_layer[layer].l_flags |= ARS_TAKE_ICMP_CKSUM;
	} else if (strcasecmp(f, "id") == 0) {
		icmp->un.echo.id = htons(ars_atou(v));
	} else if (strcasecmp(f, "seq") == 0) {
		icmp->un.echo.sequence = htons(ars_atou(v));
	} else if (strcasecmp(f, "gw") == 0) {
		return ars_resolve(pkt, &icmp->un.gateway, v);
	} else {
		ars_set_error(pkt, "Invalid field for ICMP layer");
		return -ARS_INVALID;
	}
	return -ARS_OK;
}

int ars_push_data(struct ars_packet *pkt, int layer, void *data, size_t size)
{
	char *p;
	int old_size;

	if (layer == ARS_LAST_LAYER)
		layer = pkt->p_layer_nr - 1;
	if (ars_valid_layer(layer) != -ARS_OK)
		return -ARS_INVALID;

	old_size = pkt->p_layer[layer].l_size;
	p = realloc(pkt->p_layer[layer].l_data, old_size + size);
	if (p == NULL)
		return -ARS_NOMEM;
	memcpy(p+old_size, data, size);
	pkt->p_layer[layer].l_data = p;
	pkt->p_layer[layer].l_size += size;
	return ARS_OK;
}

#define ARS_DATA_BUF_SIZE 4096
int ars_d_set_data(struct ars_packet *pkt, int layer, char *f, char *v)
{
	if (layer == ARS_LAST_LAYER)
		layer = pkt->p_layer_nr - 1;
	if (ars_valid_layer(layer) != -ARS_OK)
		return -ARS_INVALID;

	if (strcasecmp(f, "file") == 0) {
		int fd, n_read;
		unsigned char buffer[ARS_DATA_BUF_SIZE];

		if ((fd = open(v, O_RDONLY)) == -1) {
			ars_set_error(pkt, "Can't open the DATA file");
			return -ARS_ERROR;
		}
		if ((n_read = read(fd, buffer, ARS_DATA_BUF_SIZE)) == -1) {
			close(fd);
			ars_set_error(pkt, "Can't read DATA from file");
			return -ARS_ERROR;
		}
		close(fd);
		if (n_read == 0)
			return -ARS_OK;
		return ars_push_data(pkt, layer, buffer, n_read);
	} else if (strcasecmp(f, "str") == 0) {
		return ars_push_data(pkt, layer, v, strlen(v));
	} else {
		ars_set_error(pkt, "Invalid field for DATA layer");
		return -ARS_INVALID;
	}
	return -ARS_OK;
}

/* A Finite state machine to build the packet using the description */
int ars_d_build(struct ars_packet *pkt, char *t)
{
	struct ars_d_keyword_info *k = NULL;
	char next[ARS_MAX_TSIZE];
	char field[ARS_MAX_TSIZE];
	int state = ARS_G_LAYER;
	int error;
	void *p;

	while ((t = ars_d_parser(t, next, ARS_MAX_TSIZE)) != NULL) {
		switch(state) {
		case ARS_G_LAYER:
			k = ars_get_keyword_by_name(next);
			if (k == NULL) {
				ars_set_error(pkt, "Unknown keyword");
				return -ARS_INVALID;
			}
			__D(printf("Adding a new layer (%s)\n", next);)
			p = k->ki_add(pkt, k->ki_opt);
			if (p == NULL)
				return -ARS_INVALID;
			state = ARS_G_OBRACE_OR_PLUS;
			break;
		case ARS_G_FIELD:
			strncpy(field, next, ARS_MAX_TSIZE);
			state = ARS_G_EQUAL;
			break;
		case ARS_G_VALUE:
			if (k->ki_set == NULL) {
				ars_set_error(pkt, "Field specified for"
					"a layer that doesn't support fields");
				return -ARS_INVALID;
			}
			error = k->ki_set(pkt, ARS_LAST_LAYER, field, next);
			if (error != -ARS_OK)
				return error;
			state = ARS_G_COMMA_OR_CBRACE;
			break;
		case ARS_G_OBRACE_OR_PLUS:
			if (next[0] == '{' && next[1] == '\0') {
				state = ARS_G_FIELD;
				break;
			} else if (next[0] == '+' && next[1] == '\0') {
				state = ARS_G_LAYER;
				break;
			} else {
				ars_set_error(pkt, "Missing brace or plus");
				return -ARS_INVALID;
			}
			break;
		case ARS_G_CBRACE:
			if (next[0] != '}' || next[1] != '\0') {
				ars_set_error(pkt, "Missing closed brace");
				return -ARS_INVALID;
			}
			state = ARS_G_LEN_OR_PLUS;
			break;
		case ARS_G_COMMA_OR_CBRACE:
			if (next[0] == '}' && next[1] == '\0') {
				state = ARS_G_LEN_OR_PLUS;
				break;
			} else if (next[0] == ',' && next[1] == '\0') {
				state = ARS_G_FIELD;
				break;
			} else {
				ars_set_error(pkt, "Missing brace or comma");
				return -ARS_INVALID;
			}
			break;
		case ARS_G_LEN_OR_PLUS:
			if (next[0] == '+' && next[1] == '\0') {
				state = ARS_G_LAYER;
				break;
			}
			error = ars_d_setlayer_size(pkt, ARS_LAST_LAYER, next);
			if (error != -ARS_OK)
				return error;
			state = ARS_G_PLUS;
			break;
		case ARS_G_PLUS:
			if (next[0] != '+' || next[1] != '\0') {
				ars_set_error(pkt, "Missing plus");
				return -ARS_INVALID;
			}
			state = ARS_G_LAYER;
			break;
		case ARS_G_EQUAL:
			if (next[0] != '=' || next[1] != '\0') {
				ars_set_error(pkt, "Missing equal");
				return -ARS_INVALID;
			}
			state = ARS_G_VALUE;
			break;
		}
	}
	if (state != ARS_G_LEN_OR_PLUS && state != ARS_G_PLUS &&
	    state != ARS_G_OBRACE_OR_PLUS) {
		ars_set_error(pkt, "Packet description truncated");
		return -ARS_INVALID;
	}
	return -ARS_OK;
}
