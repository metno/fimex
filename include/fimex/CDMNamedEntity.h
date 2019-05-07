/*
 * Fimex
 *
 * (C) Copyright 2008, met.no
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

#ifndef CDMNAMEDENTITY_H_
#define CDMNAMEDENTITY_H_

#include <functional>
#include <memory>
#include <string>

namespace MetNoFimex
{


/**
 * @headerfile fimex/CDMNamedEntity.h
 */
/**
 * interface for all CDM Entities (variable, attribute, dimension)
 * which support the 'getName' method
 */
class CDMNamedEntity
{
public:
    virtual ~CDMNamedEntity() = 0;
    virtual const std::string& getName() const = 0;
};

/**
 * functor to compares names of two CDMNamedEntity using std::string::compare
 */
struct CDMNameCompare : public std::binary_function<CDMNamedEntity, CDMNamedEntity, int>
{
public:
    int operator()(const CDMNamedEntity& e1, const CDMNamedEntity& e2) {return e1.getName().compare(e2.getName());}
};

/**
 * functor to find a CDMNamedEntity equal to the set name using std::string::operator==
 */
class CDMNameEqual : public std::unary_function<CDMNamedEntity, bool>
{
public:
    explicit CDMNameEqual(std::string name) : name(name) {}
    explicit CDMNameEqual(const CDMNamedEntity& entity) : name(entity.getName()) {}
    ~CDMNameEqual() {}
    bool operator()(const CDMNamedEntity& e) {return name == e.getName();}
private:
    std::string name;
};

/**
 * functor to find a std::shared_ptr<CDMNamedEntity> equal to the set name using std::string::operator==
 */
class CDMNameEqualPtr : public std::unary_function<std::shared_ptr<CDMNamedEntity>, bool>
{
public:
    explicit CDMNameEqualPtr(std::string name) : name(name) {}
    explicit CDMNameEqualPtr(const std::shared_ptr<const CDMNamedEntity>& entity)
        : name(entity->getName())
    {
    }
    ~CDMNameEqualPtr() {}
    bool operator()(const std::shared_ptr<const CDMNamedEntity>& e) { return name == e->getName(); }

private:
    std::string name;
};

}

#endif /*CDMNAMEDENTITY_H_*/
