/*
 * Copyright 2014 Chris Young <chris@unsatisfactorysoftware.co.uk>
 *
 * This file is part of NetSurf, http://www.netsurf-browser.org/
 *
 * NetSurf is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * NetSurf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <proto/dos.h>
#include <proto/exec.h>

#include "content/fs_backing_store.c"

#ifdef AMIGA_NS_ASYNC
struct ami_bsm_store {
	nsurl *url;
	enum backing_store_flags flags;
	uint8_t *data;
	size_t datalen;
};

struct ami_bsm_fetch {
	nsurl *url;
	enum backing_store_flags bsflags;
	uint8_t **data_out;
	size_t *datalen_out;
};

struct ami_bsm_invalidate {
	nsurl *url;
};

struct ami_bsm_release {
	nsurl *url;
	enum backing_store_flags bsflags;
};

struct ami_backing_store {
	struct MsgPort *msgport;
	struct llcache_store_parameters *parameters;
};

struct ami_backing_store_msg {
	struct Message msg;
	int type;
	nserror error;
	void *data;
};

enum {
	AMI_BSM_STARTUP = 0,
	AMI_BSM_STORE,
	AMI_BSM_FETCH,
	AMI_BSM_INVALIDATE,
	AMI_BSM_RELEASE,
	AMI_BSM_FINALISE
};

struct MsgPort *cachemsgport = NULL;

static int32 ami_backing_store_process(STRPTR args, int32 length, APTR execbase)
{
	struct Process *proc = (struct Process *)FindTask(NULL);
	struct ami_backing_store *abs = proc->pr_Task.tc_UserData;
	struct MsgPort *nsmsgport = abs->msgport;
	struct MsgPort *backingstoremsgport = AllocSysObjectTags(ASOT_PORT, TAG_END);
	bool running = true;

	nserror error = filesystem_llcache_table->initialise(abs->parameters);

	/* Send a startup message to the message port we were given when we were created.
	 * This tells NetSurf where to send disk cache messages to, as well as informing
	 * that we are running.
	 */

	struct ami_backing_store_msg *absmsg = AllocSysObjectTags(ASOT_MESSAGE,
          ASOMSG_Size, sizeof(struct ami_backing_store_msg),
          ASOMSG_ReplyPort, backingstoremsgport,
          TAG_END);

	absmsg->type = AMI_BSM_STARTUP;
	absmsg->error = error;

	PutMsg(nsmsgport, (struct Message *)absmsg);

	/* Main loop for this process */

	while(running) {
		WaitPort(backingstoremsgport);

		while((absmsg = (struct ami_backing_store_msg *)GetMsg(backingstoremsgport))) {
			if(absmsg->msg.mn_Node.ln_Type == NT_REPLYMSG) {
				/* if it's a reply, free stuff */
				FreeSysObject(ASOT_MESSAGE, absmsg);
			} else {
				if(running) {
					switch(absmsg->type) {
						case AMI_BSM_STORE:
						{
							struct ami_bsm_store *absm = absmsg->data;
							absmsg->error = filesystem_llcache_table->store(absm->url, absm->flags, absm->data, absm->datalen);
							FreeVec(absm);
							FreeSysObject(ASOT_MESSAGE, absmsg); /* don't reply, just free */
						}
						break;

						case AMI_BSM_FETCH:
						{
							struct ami_bsm_fetch *absm = absmsg->data;
							absmsg->error = filesystem_llcache_table->fetch(absm->url, absm->bsflags, absm->data_out, absm->datalen_out);
							ReplyMsg((struct Message *)absmsg); /* need to reply to this one */
						}
						break;

						case AMI_BSM_INVALIDATE:
						{
							struct ami_bsm_invalidate *absm = absmsg->data;
							absmsg->error = filesystem_llcache_table->invalidate(absm->url);
							FreeVec(absm);
							FreeSysObject(ASOT_MESSAGE, absmsg); /* don't reply, just free */
						}
						break;

						case AMI_BSM_RELEASE:
						{
							struct ami_bsm_release *absm = absmsg->data;
							absmsg->error = filesystem_llcache_table->release(absm->url, absm->bsflags);
							FreeVec(absm);
							FreeSysObject(ASOT_MESSAGE, absmsg); /* don't reply, just free */
						}
						break;

						case AMI_BSM_FINALISE:
							running = false;
							absmsg->error = filesystem_llcache_table->finalise();
							ReplyMsg((struct Message *)absmsg); /* need to reply to this one */
						break;

						default:
							// unknown message
							FreeSysObject(ASOT_MESSAGE, absmsg); /* don't reply, just free */
						break;
					}
				}
			}
		}
	}

	FreeSysObject(ASOT_PORT, backingstoremsgport);

	return RETURN_OK;
}

static nserror ami_backing_store_send_reply(int type, void *data, struct MsgPort *msgport)
{
	if(cachemsgport == NULL) return NSERROR_INIT_FAILED;

	struct ami_backing_store_msg *absmsg = AllocSysObjectTags(ASOT_MESSAGE,
		ASOMSG_Size, sizeof(struct ami_backing_store_msg),
		ASOMSG_ReplyPort, msgport,
		TAG_END);

	absmsg->type = type;
	absmsg->data = data;

	PutMsg(cachemsgport, (struct Message *)absmsg);

	return NSERROR_OK;
}

static nserror ami_backing_store_send(int type, void *data)
{
	return ami_backing_store_send_reply(type, data, NULL);
}

/**
 * Place an object in the backing store.
 *
 * @param url The url is used as the unique primary key for the data.
 * @param flags The flags to control how the object is stored.
 * @param data The objects source data.
 * @param datalen The length of the \a data.
 * @return NSERROR_OK on success or error code on faliure.
 */
static nserror
ami_backing_store_store(nsurl *url,
      enum backing_store_flags flags,
      uint8_t *data,
      const size_t datalen)
{
	struct ami_bsm_store *absm =
		AllocVecTagList(sizeof(struct ami_bsm_store), NULL);

	if(absm == NULL) return NSERROR_NOMEM;

	absm->url = url;
	absm->flags = flags;
	absm->data = data;
	absm->datalen = datalen;

	return ami_backing_store_send(AMI_BSM_STORE, absm);
}

/**
 * Retrive an object from the backing store.
 *
 * @param[in] url The url is used as the unique primary key for the data.
 * @param[in] bsflags The flags to control how the object is retrieved.
 * @param[out] data_out The objects data.
 * @param[out] datalen_out The length of the \a data retrieved.
 * @return NSERROR_OK on success or error code on faliure.
 */
static nserror ami_backing_store_fetch(nsurl *url,
      enum backing_store_flags bsflags,
      uint8_t **data_out,
      size_t *datalen_out)
{
	struct MsgPort *tempmsgport = AllocSysObjectTags(ASOT_PORT, TAG_END);
	if(tempmsgport == NULL) return NSERROR_NOMEM;

	struct ami_bsm_fetch *absm =
		AllocVecTagList(sizeof(struct ami_bsm_fetch), NULL);
	if(absm == NULL) return NSERROR_NOMEM;

	absm->url = url;
	absm->bsflags = bsflags;
	absm->data_out = data_out;
	absm->datalen_out = datalen_out;

	nserror error = ami_backing_store_send_reply(AMI_BSM_FETCH, absm, tempmsgport);
	if(error != NSERROR_OK) return error;

	WaitPort(tempmsgport);

	struct ami_backing_store_msg *absmsg = (struct ami_backing_store_msg *)GetMsg(tempmsgport);
	error = absmsg->error;

	FreeVec(absm);
	FreeSysObject(ASOT_MESSAGE, absmsg);
	FreeSysObject(ASOT_PORT, tempmsgport);

	return error;
}


/**
 * release a previously fetched or stored memory object.
 *
 * @param[in] url The url is used as the unique primary key to invalidate.
 * @param[in] bsflags The flags to control how the object data is released.
 * @return NSERROR_OK on success or error code on faliure.
 */
static nserror ami_backing_store_release(nsurl *url, enum backing_store_flags bsflags)
{
	struct ami_bsm_release *absm =
		AllocVecTagList(sizeof(struct ami_bsm_release), NULL);

	if(absm == NULL) return NSERROR_NOMEM;

	absm->url = url;
	absm->bsflags = bsflags;

	return ami_backing_store_send(AMI_BSM_RELEASE, absm);
}

/**
 * Invalidate a source object from the backing store.
 *
 * The entry (if present in the backing store) must no longer
 * be returned as a result to the fetch or meta operations.
 *
 * @param url The url is used as the unique primary key to invalidate.
 * @return NSERROR_OK on success or error code on faliure.
 */
static nserror ami_backing_store_invalidate(nsurl *url)
{
	struct ami_bsm_store *absm =
		AllocVecTagList(sizeof(struct ami_bsm_invalidate), NULL);

	if(absm == NULL) return NSERROR_NOMEM;

	absm->url = url;

	return ami_backing_store_send(AMI_BSM_INVALIDATE, absm);
}

/**
 * Finalise the backing store.
 *
 * \todo This will cause the backing store to leak any outstanding memory
 * allocations. This will probably best be done by a global use count.
 *
 * @return NSERROR_OK on success.
 */
static nserror ami_backing_store_finalise(void)
{
	struct MsgPort *tempmsgport = AllocSysObjectTags(ASOT_PORT, TAG_END);
	if(tempmsgport == NULL) return NSERROR_NOMEM;

	nserror error = ami_backing_store_send_reply(AMI_BSM_FINALISE, NULL, tempmsgport);
	if(error != NSERROR_OK) return error;

	LOG(("Waiting for backing store process to exit..."));

	WaitPort(tempmsgport);

	struct ami_backing_store_msg *absmsg = (struct ami_backing_store_msg *)GetMsg(tempmsgport);
	error = absmsg->error;

	FreeSysObject(ASOT_MESSAGE, absmsg);
	FreeSysObject(ASOT_PORT, tempmsgport);
	cachemsgport = NULL;

	return error;
}


/**
 * Initialise the backing store.
 *
 * @param parameters to configure backing store.
 * @return NSERROR_OK on success or error code on faliure.
 */
static nserror
ami_backing_store_initialise(const struct llcache_store_parameters *parameters)
{
	struct MsgPort *tempmsgport = AllocSysObjectTags(ASOT_PORT, TAG_END);
	struct ami_backing_store *abs =
		AllocVecTagList(sizeof(struct ami_backing_store), NULL);
	if(abs == NULL) return NSERROR_NOMEM;

	abs->msgport = tempmsgport;
	abs->parameters = (struct llcache_store_parameters *)parameters;

	struct Process *proc = CreateNewProcTags(
		NP_Name, "NetSurf backing store",
		NP_Entry, ami_backing_store_process,
		NP_Child, TRUE,
		NP_StackSize, 16384,
		NP_Priority, -1,
		NP_UserData, abs,
		TAG_DONE);

	if(proc == NULL) {
		return NSERROR_NOMEM;
	}

	LOG(("Waiting for backing store process to start up..."));

	WaitPort(tempmsgport);

	struct ami_backing_store_msg *msg = (struct ami_backing_store_msg *)GetMsg(tempmsgport);
	cachemsgport = msg->msg.mn_ReplyPort;
	nserror error = msg->error;
	ReplyMsg((struct Message *)msg);
	FreeSysObject(ASOT_PORT, tempmsgport);

	LOG(("Backing store process started.  Error code: %d", error));

	return error;
}
#endif

static struct gui_llcache_table amiga_llcache_table = {
#ifndef AMIGA_NS_ASYNC
	.initialise = initialise,
	.finalise = finalise,
	.store = store,
	.fetch = fetch,
	.invalidate = invalidate,
	.release = release,
#else
	.initialise = ami_backing_store_initialise,
	.finalise = ami_backing_store_finalise,
	.store = ami_backing_store_store,
	.fetch = ami_backing_store_fetch,
	.invalidate = ami_backing_store_invalidate,
	.release = ami_backing_store_release,
#endif
};

struct gui_llcache_table *amiga_filesystem_llcache_table = &amiga_llcache_table;

