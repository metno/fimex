/*
 * Fimex
 *
 * (C) Copyright 2011, met.no
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
 */

// internals
//
#include "MetGmHandlePtr.h"
#include "MetGmFileHandlePtr.h"
#include "MetGmGroup3Ptr.h"

// METGM C Lib
//
#include "metgm.h"

// standard
//
#include <iostream>

namespace MetNoFimex {

MetGmGroup3Ptr::MetGmGroup3Ptr(const std::shared_ptr<MetGmHandlePtr> pHandle)
    : pHandle_(pHandle)
    , group3_(0)
{
    assert(pHandle_.get());
    group3_ = mgm_new_group3();
    assert(group3_);
    }

    MetGmGroup3Ptr::MetGmGroup3Ptr(const std::shared_ptr<MetGmHandlePtr> pHandle, const unsigned short p_id)
        : pHandle_(pHandle)
        , group3_(0)
    {
        assert(pHandle_.get());
        group3_ = mgm_new_group3();
        assert(group3_);
        MGM_THROW_ON_ERROR(mgm_set_p_id(group3_, p_id));
    }

    std::shared_ptr<MetGmGroup3Ptr> MetGmGroup3Ptr::createMetGmGroup3PtrForWriting(const std::shared_ptr<MetGmHandlePtr> pHandle, const unsigned short p_id)
    {
        assert(pHandle.get());
        std::shared_ptr<MetGmGroup3Ptr> gp3 = std::shared_ptr<MetGmGroup3Ptr>(new MetGmGroup3Ptr(pHandle, p_id));
        return gp3;
    }

    MetGmGroup3Ptr::~MetGmGroup3Ptr()
    {
        if(group3_)
            mgm_free_group3(group3_);
    }

    const std::shared_ptr<MetGmHandlePtr>& MetGmGroup3Ptr::mgmHandle()
    {
        return pHandle_;
    }

    std::shared_ptr<MetGmGroup3Ptr> MetGmGroup3Ptr::createMetGmGroup3PtrForReading(const std::shared_ptr<MetGmHandlePtr> pHandle)
    {
        assert(pHandle.get());
        std::shared_ptr<MetGmGroup3Ptr> pg3 = std::shared_ptr<MetGmGroup3Ptr>(new MetGmGroup3Ptr(pHandle));
        mgm_read_group3(*pg3->pHandle_->fileHandle(), *pg3->mgmHandle(), *pg3);
        return pg3;
    }

    int MetGmGroup3Ptr::reset()
    {
        return mgm_reset_group3(group3_);
    }

    int MetGmGroup3Ptr::p_id() const
    {
        return mgm_get_p_id(group3_);
    }

    int MetGmGroup3Ptr::nz() const
    {
        return mgm_get_nz(group3_);
    }

    int MetGmGroup3Ptr::nx() const
    {
        return mgm_get_nx(group3_);
    }

    int MetGmGroup3Ptr::ny() const
    {
        return mgm_get_ny(group3_);
    }

    int MetGmGroup3Ptr::nt() const
    {
        return mgm_get_nt(group3_);
    }

    float MetGmGroup3Ptr::dx() const
    {
        return mgm_get_dx(group3_);
    }

    float MetGmGroup3Ptr::dy() const
    {
        return mgm_get_dy(group3_);
    }

    float MetGmGroup3Ptr::dt() const
    {
        return mgm_get_dt(group3_);
    }

    float MetGmGroup3Ptr::cx() const
    {
        return mgm_get_cx(group3_);
    }

    float MetGmGroup3Ptr::cy() const
    {
        return mgm_get_cy(group3_);
    }

    short MetGmGroup3Ptr::pr() const
    {
        return mgm_get_pr(group3_);
    }

    short MetGmGroup3Ptr::pz() const
    {
        return mgm_get_pz(group3_);
    }

    int MetGmGroup3Ptr::set_p_id(int p_id)
    {
        return mgm_set_p_id(group3_, p_id);
    }

    int MetGmGroup3Ptr::set_nz(int nz)
    {
        return mgm_set_nz(group3_, nz);
    }

    int MetGmGroup3Ptr::set_nx(int nx)
    {
        return mgm_set_nx(group3_, nx);
    }

    int MetGmGroup3Ptr::set_ny(int ny)
    {
        return mgm_set_ny(group3_, ny);
    }

    int MetGmGroup3Ptr::set_nt(int nt)
    {
        return mgm_set_nt(group3_, nt);
    }

    int MetGmGroup3Ptr::set_dx(float dx)
    {
        return mgm_set_dx(group3_, dx);
    }

    int MetGmGroup3Ptr::set_dy(float dy)
    {
        return mgm_set_dy(group3_, dy);
    }

    int MetGmGroup3Ptr::set_dt(float dt)
    {
        return mgm_set_dt(group3_, dt);
    }

    int MetGmGroup3Ptr::set_cx(float cx)
    {
        return mgm_set_cx(group3_, cx);
    }

    int MetGmGroup3Ptr::set_cy(float cy)
    {
        return mgm_set_cy(group3_, cy);
    }

    int MetGmGroup3Ptr::set_pr(short pr)
    {
        return mgm_set_pr(group3_, pr);
    }

    int MetGmGroup3Ptr::set_pz(short pz)
    {
        return mgm_set_pz(group3_, pz);
    }

    bool MetGmGroup3Ptr::eq(std::shared_ptr<MetGmGroup3Ptr>& rhs) const
    {
        if(this == rhs.get()) return true;

        if(        nz() == rhs->nz()
                && nx() == rhs->nx()
                && ny() == rhs->ny()
                && nt() == rhs->nt()
                && dx() == rhs->dx()
                && dy() == rhs->dy()
                && dt() == rhs->dt()
                && cx() == rhs->cx()
                && cy() == rhs->cy()
                && pr() == rhs->pr()
                && pz() == rhs->pz()
                )
            return true;
        else
            return false;
    }

    bool MetGmGroup3Ptr::neq(std::shared_ptr<MetGmGroup3Ptr>& rhs) const
    {
        return !(eq(rhs));
    }

    void MetGmGroup3Ptr::dump() {
        std::cerr << "dumping group3 [START]" << std::endl
                  << "p_id = " << p_id()      << std::endl
                  << "nz = "   << nz()        << std::endl
                  << "nx = "   << nx()        << std::endl
                  << "ny = "   << ny()        << std::endl
                  << "nt = "   << nt()        << std::endl
                  << "dx = "   << dx()        << std::endl
                  << "dy = "   << dy()        << std::endl
                  << "dt = "   << dt()        << std::endl
                  << "cx = "   << cx()        << std::endl
                  << "cy = "   << cy()        << std::endl
                  << "pr = "   << pr()        << std::endl
                  << "pz = "   << pz()        << std::endl
                  << "dumping group3 [END]"   << std::endl;
    }

}
