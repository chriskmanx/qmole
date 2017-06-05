//========================================================================
//
// StdinCachedFile.cc
//
// This file is licensed under the GPLv2 or later
//
// Copyright 2010 Hib Eris <hib@hiberis.nl>
// Copyright 2010 Albert Astals Cid <aacid@kde.org>
//
//========================================================================

#include <config.h>

#include "StdinCachedFile.h"

#include <stdio.h>

size_t StdinCacheLoader::init(GooString *dummy, CachedFile *cachedFile)
{
  size_t read, size = 0;
  char buf[CachedFileChunkSize];

  CachedFileWriter writer = CachedFileWriter (cachedFile, NULL);
  do {
    read = fread(buf, 1, CachedFileChunkSize, stdin);
    (writer.write) (buf, CachedFileChunkSize);
    size += read;
  }
  while (read == CachedFileChunkSize);

  return size;
}

int StdinCacheLoader::load(const GooVector<ByteRange> &ranges, CachedFileWriter *writer)
{
  return 0;
}

