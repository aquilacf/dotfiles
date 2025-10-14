#!/bin/sh
#
# GitHub Enterprise Repository Backup Script
# 
# This script retrieves all organizations from a GitHub enterprise,
# lists all repositories for each organization, and clones/updates them.
#
# Requirements:
#   - curl (for API requests)
#   - jq (for JSON parsing)
#   - git (for repository operations)
#
# Environment Variables:
#   GITHUB_TOKEN    - GitHub Personal Access Token (required)
#   GITHUB_API_URL  - GitHub GraphQL API URL (default: https://api.github.com/graphql)
#   ENTERPRISE_SLUG - GitHub Enterprise slug (required for enterprise queries)
#

set -e  # Exit on any error

# Configuration
GITHUB_API_URL="${GITHUB_API_URL:-https://api.github.com/graphql}"
DRY_RUN="${DRY_RUN:-false}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    printf "${BLUE}[INFO]${NC} %s\n" "$1" >&2
}

log_success() {
    printf "${GREEN}[SUCCESS]${NC} %s\n" "$1" >&2
}

log_warning() {
    printf "${YELLOW}[WARNING]${NC} %s\n" "$1" >&2
}

log_error() {
    printf "${RED}[ERROR]${NC} %s\n" "$1" >&2
}

# Usage information
usage() {
    cat << EOF
Usage: $0 [OPTIONS]

GitHub Enterprise Repository Backup Script (GraphQL API)

OPTIONS:
    -h, --help          Show this help message
    -d, --dry-run       Show what would be done without executing
    -e, --enterprise    Enterprise slug (required for GraphQL queries)
    -u, --url URL       GitHub GraphQL API URL (default: https://api.github.com/graphql)

Environment Variables:
    GITHUB_TOKEN        GitHub Personal Access Token (required)
    GITHUB_API_URL      GitHub GraphQL API URL
    ENTERPRISE_SLUG     GitHub Enterprise slug (required)
    DRY_RUN             Set to 'true' for dry run mode

Examples:
    # Basic usage with environment variables
    export GITHUB_TOKEN="ghp_xxxxxxxxxxxx"
    export ENTERPRISE_SLUG="my-enterprise"
    $0

    # With command line options
    $0 --enterprise "my-enterprise"

    # Dry run to see what would happen
    $0 --dry-run --enterprise "my-enterprise"

    # For GitHub Enterprise Server
    $0 --url "https://github.company.com/api/graphql" --enterprise "my-enterprise"

Note: Repositories are cloned using SSH. Ensure your SSH keys are configured properly.

EOF
}

# Check dependencies
check_dependencies() {
    local missing_deps=""
    
    for cmd in curl jq git; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            missing_deps="$missing_deps $cmd"
        fi
    done
    
    if [ -n "$missing_deps" ]; then
        log_error "Missing required dependencies:$missing_deps"
        log_error "Please install the missing tools and try again."
        exit 1
    fi
}

# Make authenticated GraphQL request
graphql_request() {
    local query="$1"
    local variables="$2"
    
    if [ -z "$GITHUB_TOKEN" ]; then
        log_error "GITHUB_TOKEN is required"
        exit 1
    fi
    
    local request_body
    if [ -n "$variables" ]; then
        request_body=$(jq -n --arg query "$query" --argjson variables "$variables" '{query: $query, variables: $variables}')
    else
        request_body=$(jq -n --arg query "$query" '{query: $query}')
    fi
    
    local response
    response=$(curl -s -w "\n%{http_code}" \
        -X POST \
        -H "Authorization: Bearer $GITHUB_TOKEN" \
        -H "Content-Type: application/json" \
        -d "$request_body" \
        "$GITHUB_API_URL")
    
    local http_code=$(echo "$response" | tail -n1)
    local body=$(echo "$response" | sed '$d')
    
    if [ "$http_code" -ne 200 ]; then
        log_error "GraphQL request failed with HTTP $http_code"
        echo "$body" >&2
        return 1
    fi
    
    echo "$body"
}

# Generic GraphQL pagination helper
graphql_paginate() {
    local query="$1"
    local data_path="$2"
    local entity_name="$3"
    local base_variables="$4"
    
    local all_results="[]"
    local after_cursor=""
    local has_next_page="true"
    
    while [ "$has_next_page" = "true" ]; do
        local variables
        if [ -n "$after_cursor" ]; then
            variables=$(echo "$base_variables" | jq --arg after "$after_cursor" '. + {after: $after}')
        else
            variables=$(echo "$base_variables" | jq '. + {after: null}')
        fi
        
        local result
        result=$(graphql_request "$query" "$variables") || return 1
        
        # Extract data from response using the provided path
        local page_data
        page_data=$(echo "$result" | jq "$data_path // []")
        
        # Filter out null nodes if they exist
        page_data=$(echo "$page_data" | jq 'map(select(. != null))')
        
        # Add to all results
        all_results=$(echo "$all_results $page_data" | jq -s 'add')
        
        # Extract pagination info - find the pageInfo path by replacing the last part with 'pageInfo'
        local pageinfo_path
        pageinfo_path=$(echo "$data_path" | sed 's/\.nodes$/.pageInfo/')
        
        # Check pagination info
        has_next_page=$(echo "$result" | jq -r "${pageinfo_path}.hasNextPage // false")
        after_cursor=$(echo "$result" | jq -r "${pageinfo_path}.endCursor // \"\"")
        
        local item_count
        item_count=$(echo "$page_data" | jq 'length')
        log_info "Fetched $item_count $entity_name (cursor: ${after_cursor:-none})"
        
        if [ "$has_next_page" != "true" ]; then
            break
        fi
    done
    
    echo "$all_results"
}

# Get all organizations using GraphQL
get_organizations() {
    local query='
        query($slug: String!, $after: String) {
            enterprise(slug: $slug) {
                name
                organizations(first: 100, after: $after) {
                    pageInfo {
                        hasNextPage
                        endCursor
                    }
                    nodes {
                        login
                    }
                }
            }
        }
    '
    
    local base_variables
    base_variables=$(jq -n --arg slug "$ENTERPRISE_SLUG" '{slug: $slug}')
    
    log_info "Fetching organizations for enterprise: $ENTERPRISE_SLUG"
    graphql_paginate "$query" ".data.enterprise.organizations.nodes" "organizations" "$base_variables"
}

# Get repositories for an organization using GraphQL
get_org_repositories() {
    local org="$1"
    
    local query='
        query($org: String!, $after: String) {
            organization(login: $org) {
                repositories(first: 100, after: $after) {
                    pageInfo {
                        hasNextPage
                        endCursor
                    }
                    nodes {
                        name
                        sshUrl
                    }
                }
            }
        }
    '
    
    local base_variables
    base_variables=$(jq -n --arg org "$org" '{org: $org}')
    
    log_info "Fetching repositories for organization: $org"
    graphql_paginate "$query" ".data.organization.repositories.nodes" "repositories for $org" "$base_variables"
}

# Get clone URL (SSH only)
get_clone_url() {
    local repo_data="$1"
    echo "$repo_data" | jq -r '.sshUrl'
}

# Create directory safely
safe_mkdir() {
    local dir="$1"
    
    if [ "$DRY_RUN" = "true" ]; then
        log_info "Would create directory: $dir"
        return 0
    fi
    
    if ! mkdir -p "$dir"; then
        log_error "Failed to create directory: $dir"
        return 1
    fi
}

# Clone or update repository
process_repository() {
    local org="$1"
    local repo_data="$2"
    local repo_name
    local clone_url
    local repo_dir
    
    repo_name=$(echo "$repo_data" | jq -r '.name')
    clone_url=$(get_clone_url "$repo_data")
    repo_dir="$org/$repo_name"
    
    log_info "Processing repository: $org/$repo_name"
    
    if [ -d "$repo_dir/.git" ]; then
        # Repository exists, update it
        log_info "Repository exists, updating: $repo_name"
        
        if [ "$DRY_RUN" = "true" ]; then
            log_info "Would update repository in: $repo_dir"
            return 0
        fi
        
        if ! (cd "$repo_dir" && git pull --recurse-submodules --rebase); then
            log_warning "Failed to update repository: $org/$repo_name"
            return 1
        fi
        
        log_success "Updated: $org/$repo_name"
    else
        # Repository doesn't exist, clone it
        log_info "Cloning new repository: $repo_name"
        
        if [ "$DRY_RUN" = "true" ]; then
            log_info "Would clone: $clone_url -> $repo_dir"
            return 0
        fi
        
        safe_mkdir "$(dirname "$repo_dir")" || return 1
        
        if ! git clone --recurse-submodules "$clone_url" "$repo_dir"; then
            log_warning "Failed to clone repository: $org/$repo_name"
            return 1
        fi
        
        log_success "Cloned: $org/$repo_name"
    fi
}

# Process all repositories for an organization
process_organization() {
    local org="$1"
    local repositories
    local repo_count=0
    local success_count=0
    
    log_info "Processing organization: $org"
    
    # Create organization directory
    safe_mkdir "$org" || return 1
    
    # Get all repositories for this organization
    repositories=$(get_org_repositories "$org") || {
        log_error "Failed to get repositories for organization: $org"
        return 1
    }
    
    repo_count=$(echo "$repositories" | jq 'length')
    log_info "Found $repo_count repositories in organization: $org"
    
    if [ "$repo_count" -eq 0 ]; then
        log_warning "No repositories found for organization: $org"
        return 0
    fi
    
    # Process each repository
    echo "$repositories" | jq -c '.[]' | while read -r repo_data; do
        if process_repository "$org" "$repo_data"; then
            success_count=$((success_count + 1))
        fi
    done
    
    log_success "Completed organization: $org"
}

# Main function
main() {
    local start_time
    local org_count=0
    local total_start_time
    
    total_start_time=$(date +%s)
    
    log_info "Starting GitHub Enterprise backup"
    log_info "API URL: $GITHUB_API_URL"
    log_info "Working directory: $(pwd)"
    log_info "Clone method: SSH"
    
    if [ "$DRY_RUN" = "true" ]; then
        log_warning "DRY RUN MODE - No actual changes will be made"
    fi
    
    # Check dependencies
    check_dependencies
    
    # Get all organizations
    log_info "Fetching organizations..."
    local organizations
    organizations=$(get_organizations) || {
        log_error "Failed to fetch organizations"
        exit 1
    }
    
    org_count=$(echo "$organizations" | jq 'length')
    log_info "Found $org_count organizations"
    
    if [ "$org_count" -eq 0 ]; then
        log_warning "No organizations found"
        exit 0
    fi
    
    # Process each organization
    echo "$organizations" | jq -r '.[].login' | while read -r org; do
        start_time=$(date +%s)
        process_organization "$org"
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        log_info "Organization $org completed in ${duration}s"
    done
    
    local total_end_time=$(date +%s)
    local total_duration=$((total_end_time - total_start_time))
    
    log_success "Backup completed in ${total_duration}s"
    log_info "Repositories stored in current directory"
}

# Parse command line arguments
while [ $# -gt 0 ]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -d|--dry-run)
            DRY_RUN="true"
            ;;
        -e|--enterprise)
            ENTERPRISE_SLUG="$2"
            shift
            ;;
        -u|--url)
            GITHUB_API_URL="$2"
            shift
            ;;
        *)
            log_error "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
    shift
done

# Validate required parameters
if [ -z "$GITHUB_TOKEN" ]; then
    log_error "GitHub token is required. Set GITHUB_TOKEN environment variable."
    exit 1
fi

if [ -z "$ENTERPRISE_SLUG" ]; then
    log_error "Enterprise slug is required. Set ENTERPRISE_SLUG environment variable or use --enterprise option."
    exit 1
fi

main "$@"
