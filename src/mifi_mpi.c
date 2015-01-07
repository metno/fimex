/*
 * Fimex, mifi_mpi.c
 *
 * (C) Copyright 2015, met.no
 *
 * Project Info:  https://wiki.met.no/fimex/start
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 *  Created on: Jan 5, 2015
 *      Author: heikok
 */

#include "../config.h"
#ifdef HAVE_MPI

#include "fimex/mifi_mpi.h"
#include <stdio.h>

static int mifi_is_initialized = 0;

MPI_Comm  mifi_mpi_comm;
MPI_Info mifi_mpi_info;
int mifi_mpi_size;
int mifi_mpi_rank;

int mifi_mpi_initialized() {
    return mifi_is_initialized;
}

void mifi_initialize_mpi(MPI_Comm comm, MPI_Info info)
{
    int mpi_namelen;
    char mpi_name[MPI_MAX_PROCESSOR_NAME];

    mifi_mpi_comm = comm;
    mifi_mpi_info = info;

    MPI_Comm_size(mifi_mpi_comm, &mifi_mpi_size);
    MPI_Comm_rank(mifi_mpi_comm, &mifi_mpi_rank);
    MPI_Get_processor_name(mpi_name, &mpi_namelen);
    printf("mpi_name: %s size: %d rank: %d\n",
           mpi_name, mifi_mpi_size, mifi_mpi_rank);

    mifi_is_initialized = 1;
}
void mifi_free_mpi()
{
    int mpi_size, mpi_rank;
    int mpi_namelen;
    char mpi_name[MPI_MAX_PROCESSOR_NAME];
    if (mifi_mpi_initialized()) {
        mifi_is_initialized = 0;
        MPI_Comm_size(mifi_mpi_comm, &mpi_size);
        MPI_Comm_rank(mifi_mpi_comm, &mpi_rank);
        MPI_Get_processor_name(mpi_name, &mpi_namelen);
        printf("finalize: mpi_name: %s size: %d rank: %d\n",
               mpi_name, mpi_size, mpi_rank);
    }
}
#endif
