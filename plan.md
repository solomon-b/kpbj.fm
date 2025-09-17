# KPBJ 95.9FM Wireframe Planning Document

## User Roles & Hierarchy
- **Public**: Listen, browse content
- **Registered Users**: Comment, like content, submit events
- **Hosts**: Upload episodes, manage show content, mini-blog
- **Staff**: Schedule management, host administration, bulk communications
- **Admins**: Content/user moderation, system settings

## Core Features
- Persistent live player across all pages
- On-demand episode playback with track listings
- User accounts for community engagement
- Multi-level content management system
- Host onboarding and management workflows

## Site Pages

### Public Pages
- **Home**: Live player, recent episodes, events, featured content
- **Listen**: Full-page live player experience
- **Shows/Schedule**: Weekly grid, show directory, individual show pages
- **Show Page**: Host bio, episode archive, host blog tabs
- **Archive**: Episode discovery with search/filter
- **Blog**: Main station blog with categories
- **Events**: Community calendar with event details
- **Store**: Merchandise and fundraising items
- **About**: Station info, staff, mission
- **Login/Register**: User authentication and account creation

### Host Dashboard
- **Content Management**: Upload episodes, edit show info, manage blog
- **Episode Upload**: Audio files with metadata, track lists, scheduling
- **Blog Editor**: Host-specific blogging system

### Staff Tools
- **Staff Dashboard**: Operations overview, quick actions, system status
- **Schedule Manager**: Weekly grid management, host assignments
- **Host Manager**: Host tracking, status management, bulk actions
- **Bulk Email**: Host communications with templates and scheduling
- **Email Manager**: Email history and management interface

### Admin Tools  
- **Add New Host**: Promote registered users to host role
- **Content Moderation**: Review content and handle reports
- **User Management**: Account administration and role management
- **System Settings**: Technical configuration

## Data Architecture
- **Time Slot → Show → Host** relationship hierarchy
- Host roles attached to user accounts (not separate entities)
- Content moderation handles both proactive review and user reports
- Email system tracks status (SENT, SCHEDULED, DRAFT) with timestamps

## Key Workflows

### Host Onboarding
1. User creates account via registration
2. Staff promotes user to Host role via Add New Host interface
3. Optional immediate show creation and time slot assignment
4. Automated welcome emails and training notifications

### Content Management
- **Host Level**: Upload episodes, manage show content, host-specific blog
- **Staff Level**: Schedule oversight, host management, bulk communications
- **Admin Level**: Content moderation, user management, system administration

### Schedule Management
- Weekly grid interface for time slot assignments
- Show-based assignments (hosts assigned to shows, shows to time slots)
- Schedule health tracking and completion metrics
- Available hosts seeking time slot assignments

### Communication System
- Bulk email with recipient filtering and templates
- Email scheduling and delivery tracking
- Comprehensive email history management
- Template system for common communications

## Technical Features
- Role-based access control (Public → Users → Hosts → Staff → Admin)
- CSS-only interactive elements where possible
- Responsive design with mobile-first approach
- Consistent branding and typography across all interfaces
- Form validation and user feedback systems

## Implementation Status
All wireframes completed for:
- Public pages (home, listen, shows, archive, blog, events, store, about)
- Authentication system (login, register)
- Host dashboard and content management
- Complete staff administration suite
- Admin-level moderation and user management tools
- Comprehensive email communication system

Total wireframes: 20+ pages covering all user roles and workflows
