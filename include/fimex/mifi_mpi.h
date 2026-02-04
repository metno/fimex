/*
 * Fimex, mifi_mpi.h
 *
 * (C) Copyright 2014-2026, met.no
 *
 * Project Info:  https://github.com/metno/fimex/wiki
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
 *  Created on: Dec 22, 2014
 *      Author: heikok
 */


#include "mpi.h"

#ifndef MIFI_MPI_H_
#define MIFI_MPI_H_


#ifdef __cplusplus
extern "C" {
#endif

extern MPI_Comm  mifi_mpi_comm;
extern MPI_Info  mifi_mpi_info;
extern int mifi_mpi_size;
extern int mifi_mpi_rank;

/**
 * Initialize mifi_mpi_* variables. Must be called after MPI_Init()
 * @param comm MPI_Communication, e.g. MPI_WORLD_COMM
 * @param info MPI_Information, e.g. MPI_INFO_NULL
 */
extern void mifi_initialize_mpi(MPI_Comm comm, MPI_Info info);
/**
 * Free/uninitialize mifi_mpi_* variables. Must be called before MPI_Finalize()
 */
extern void mifi_free_mpi();
/**
 * check if mifi_initialize_mpi has been called before
 * @return 1 on success, 0 on false
 */
extern int mifi_mpi_initialized();

#ifdef __cplusplus
}
#endif

#endif /* MIFI_MPI_H_ */
