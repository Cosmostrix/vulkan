{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- LANGUAGE DuplicateRecordFields #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2016 Cosmostrix
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Cosmostrix <cosmos@lunakit.org>
-- Stability   :  experimental
-- Portability :  DataKinds, PatternSynonyms, GeneralizedNewtypeDeriving
-- 
-- <<Vulkan_170px_Mar15.png>>
-- 
-- Complete <https://www.khronos.org/vulkan/ Vulkan> raw API bindings.
-- 
-- * <https://www.khronos.org/assets/uploads/developers/library/overview/vulkan-overview.pdf Overview presentation>
-- * <https://www.khronos.org/registry/vulkan Vulkan Registry>
-- * <https://www.khronos.org/registry/vulkan/specs/1.0/refguide/Vulkan-1.0-web.pdf Online Quick Reference>
-- * <https://github.com/KhronosGroup/Khronosdotorg/blob/master/api/vulkan/resources.md Vulkan Resources on Github>
-- * <https://www.khronos.org/registry/vulkan/specs/1.0-wsi_extensions/pdf/vkspec.pdf Vulkan 1.0 Core API + WSI Extensions (PDF)>
-- Vulkan and the Vulkan logo are trademarks of the Khronos Group Inc.
----------------------------------------------------------------------------
module Graphics.Vulkan.Bindings (
  -- * Initialization
    castToFixedString
  , getVulkanSetup
  , getVulkan
  , VulkanSetup(..)
  , Vulkan(..)
  -- * Misc
  , pattern VK_HEADER_VERSION
  , pattern VK_LOD_CLAMP_NONE
  , pattern VK_REMAINING_MIP_LEVELS
  , pattern VK_REMAINING_ARRAY_LAYERS
  , pattern VK_WHOLE_SIZE
  , pattern VK_ATTACHMENT_UNUSED
  , pattern VK_TRUE
  , pattern VK_FALSE
  , pattern VK_QUEUE_FAMILY_IGNORED
  , pattern VK_SUBPASS_EXTERNAL
  , VkFlags
  --, Display
  --, VisualID
  --, Window
  --, ANativeWindow
  --, MirConnection
  --, MirSurface
  --, WlDisplay
  --, WlSurface
  --, HINSTANCE
  --, HWND
  --, XcbConnection
  --, XcbVisualId
  --, XcbWindow
  -- * VK_VERSION_1_0
  -- ** Device initialization
  , vkCreateInstance
  , VkInstanceCreateInfo(..)
  , VkStructureType
  , pattern VK_STRUCTURE_TYPE_APPLICATION_INFO
  , pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
  , VkInstanceCreateFlags
  , VkApplicationInfo(..)
  , VkAllocationCallbacks(..)
  , VkInstance
  , vkDestroyInstance
  , vkEnumeratePhysicalDevices
  , VkPhysicalDevice
  , vkGetPhysicalDeviceFeatures
  , VkPhysicalDeviceFeatures(..)
  , VkBool32
  , vkGetPhysicalDeviceFormatProperties
  , VkFormat
  , pattern VK_FORMAT_UNDEFINED
  , pattern VK_FORMAT_R4G4_UNORM_PACK8
  , pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16
  , pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16
  , pattern VK_FORMAT_R5G6B5_UNORM_PACK16
  , pattern VK_FORMAT_B5G6R5_UNORM_PACK16
  , pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16
  , pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16
  , pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16
  , pattern VK_FORMAT_R8_UNORM
  , pattern VK_FORMAT_R8_SNORM
  , pattern VK_FORMAT_R8_USCALED
  , pattern VK_FORMAT_R8_SSCALED
  , pattern VK_FORMAT_R8_UINT
  , pattern VK_FORMAT_R8_SINT
  , pattern VK_FORMAT_R8_SRGB
  , pattern VK_FORMAT_R8G8_UNORM
  , pattern VK_FORMAT_R8G8_SNORM
  , pattern VK_FORMAT_R8G8_USCALED
  , pattern VK_FORMAT_R8G8_SSCALED
  , pattern VK_FORMAT_R8G8_UINT
  , pattern VK_FORMAT_R8G8_SINT
  , pattern VK_FORMAT_R8G8_SRGB
  , pattern VK_FORMAT_R8G8B8_UNORM
  , pattern VK_FORMAT_R8G8B8_SNORM
  , pattern VK_FORMAT_R8G8B8_USCALED
  , pattern VK_FORMAT_R8G8B8_SSCALED
  , pattern VK_FORMAT_R8G8B8_UINT
  , pattern VK_FORMAT_R8G8B8_SINT
  , pattern VK_FORMAT_R8G8B8_SRGB
  , pattern VK_FORMAT_B8G8R8_UNORM
  , pattern VK_FORMAT_B8G8R8_SNORM
  , pattern VK_FORMAT_B8G8R8_USCALED
  , pattern VK_FORMAT_B8G8R8_SSCALED
  , pattern VK_FORMAT_B8G8R8_UINT
  , pattern VK_FORMAT_B8G8R8_SINT
  , pattern VK_FORMAT_B8G8R8_SRGB
  , pattern VK_FORMAT_R8G8B8A8_UNORM
  , pattern VK_FORMAT_R8G8B8A8_SNORM
  , pattern VK_FORMAT_R8G8B8A8_USCALED
  , pattern VK_FORMAT_R8G8B8A8_SSCALED
  , pattern VK_FORMAT_R8G8B8A8_UINT
  , pattern VK_FORMAT_R8G8B8A8_SINT
  , pattern VK_FORMAT_R8G8B8A8_SRGB
  , pattern VK_FORMAT_B8G8R8A8_UNORM
  , pattern VK_FORMAT_B8G8R8A8_SNORM
  , pattern VK_FORMAT_B8G8R8A8_USCALED
  , pattern VK_FORMAT_B8G8R8A8_SSCALED
  , pattern VK_FORMAT_B8G8R8A8_UINT
  , pattern VK_FORMAT_B8G8R8A8_SINT
  , pattern VK_FORMAT_B8G8R8A8_SRGB
  , pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32
  , pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32
  , pattern VK_FORMAT_A8B8G8R8_UINT_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SINT_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32
  , pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32
  , pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32
  , pattern VK_FORMAT_A2R10G10B10_UINT_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SINT_PACK32
  , pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32
  , pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32
  , pattern VK_FORMAT_A2B10G10R10_UINT_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SINT_PACK32
  , pattern VK_FORMAT_R16_UNORM
  , pattern VK_FORMAT_R16_SNORM
  , pattern VK_FORMAT_R16_USCALED
  , pattern VK_FORMAT_R16_SSCALED
  , pattern VK_FORMAT_R16_UINT
  , pattern VK_FORMAT_R16_SINT
  , pattern VK_FORMAT_R16_SFLOAT
  , pattern VK_FORMAT_R16G16_UNORM
  , pattern VK_FORMAT_R16G16_SNORM
  , pattern VK_FORMAT_R16G16_USCALED
  , pattern VK_FORMAT_R16G16_SSCALED
  , pattern VK_FORMAT_R16G16_UINT
  , pattern VK_FORMAT_R16G16_SINT
  , pattern VK_FORMAT_R16G16_SFLOAT
  , pattern VK_FORMAT_R16G16B16_UNORM
  , pattern VK_FORMAT_R16G16B16_SNORM
  , pattern VK_FORMAT_R16G16B16_USCALED
  , pattern VK_FORMAT_R16G16B16_SSCALED
  , pattern VK_FORMAT_R16G16B16_UINT
  , pattern VK_FORMAT_R16G16B16_SINT
  , pattern VK_FORMAT_R16G16B16_SFLOAT
  , pattern VK_FORMAT_R16G16B16A16_UNORM
  , pattern VK_FORMAT_R16G16B16A16_SNORM
  , pattern VK_FORMAT_R16G16B16A16_USCALED
  , pattern VK_FORMAT_R16G16B16A16_SSCALED
  , pattern VK_FORMAT_R16G16B16A16_UINT
  , pattern VK_FORMAT_R16G16B16A16_SINT
  , pattern VK_FORMAT_R16G16B16A16_SFLOAT
  , pattern VK_FORMAT_R32_UINT
  , pattern VK_FORMAT_R32_SINT
  , pattern VK_FORMAT_R32_SFLOAT
  , pattern VK_FORMAT_R32G32_UINT
  , pattern VK_FORMAT_R32G32_SINT
  , pattern VK_FORMAT_R32G32_SFLOAT
  , pattern VK_FORMAT_R32G32B32_UINT
  , pattern VK_FORMAT_R32G32B32_SINT
  , pattern VK_FORMAT_R32G32B32_SFLOAT
  , pattern VK_FORMAT_R32G32B32A32_UINT
  , pattern VK_FORMAT_R32G32B32A32_SINT
  , pattern VK_FORMAT_R32G32B32A32_SFLOAT
  , pattern VK_FORMAT_R64_UINT
  , pattern VK_FORMAT_R64_SINT
  , pattern VK_FORMAT_R64_SFLOAT
  , pattern VK_FORMAT_R64G64_UINT
  , pattern VK_FORMAT_R64G64_SINT
  , pattern VK_FORMAT_R64G64_SFLOAT
  , pattern VK_FORMAT_R64G64B64_UINT
  , pattern VK_FORMAT_R64G64B64_SINT
  , pattern VK_FORMAT_R64G64B64_SFLOAT
  , pattern VK_FORMAT_R64G64B64A64_UINT
  , pattern VK_FORMAT_R64G64B64A64_SINT
  , pattern VK_FORMAT_R64G64B64A64_SFLOAT
  , pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32
  , pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32
  , pattern VK_FORMAT_D16_UNORM
  , pattern VK_FORMAT_X8_D24_UNORM_PACK32
  , pattern VK_FORMAT_D32_SFLOAT
  , pattern VK_FORMAT_S8_UINT
  , pattern VK_FORMAT_D16_UNORM_S8_UINT
  , pattern VK_FORMAT_D24_UNORM_S8_UINT
  , pattern VK_FORMAT_D32_SFLOAT_S8_UINT
  , pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK
  , pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK
  , pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK
  , pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK
  , pattern VK_FORMAT_BC2_UNORM_BLOCK
  , pattern VK_FORMAT_BC2_SRGB_BLOCK
  , pattern VK_FORMAT_BC3_UNORM_BLOCK
  , pattern VK_FORMAT_BC3_SRGB_BLOCK
  , pattern VK_FORMAT_BC4_UNORM_BLOCK
  , pattern VK_FORMAT_BC4_SNORM_BLOCK
  , pattern VK_FORMAT_BC5_UNORM_BLOCK
  , pattern VK_FORMAT_BC5_SNORM_BLOCK
  , pattern VK_FORMAT_BC6H_UFLOAT_BLOCK
  , pattern VK_FORMAT_BC6H_SFLOAT_BLOCK
  , pattern VK_FORMAT_BC7_UNORM_BLOCK
  , pattern VK_FORMAT_BC7_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK
  , pattern VK_FORMAT_EAC_R11_UNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11_SNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK
  , pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK
  , VkFormatProperties(..)
  , VkFormatFeatureFlags
  , vkGetPhysicalDeviceImageFormatProperties
  , VkImageType
  , pattern VK_IMAGE_TYPE_1D
  , pattern VK_IMAGE_TYPE_2D
  , pattern VK_IMAGE_TYPE_3D
  , VkImageTiling
  , pattern VK_IMAGE_TILING_OPTIMAL
  , pattern VK_IMAGE_TILING_LINEAR
  , VkImageUsageFlags
  , VkImageCreateFlags
  , VkImageFormatProperties(..)
  , VkExtent3D(..)
  , VkSampleCountFlags
  , VkDeviceSize
  , vkGetPhysicalDeviceProperties
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceType
  , pattern VK_PHYSICAL_DEVICE_TYPE_OTHER
  , pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_CPU
  , VkPhysicalDeviceLimits(..)
  , VkPhysicalDeviceSparseProperties(..)
  , vkGetPhysicalDeviceQueueFamilyProperties
  , VkQueueFamilyProperties(..)
  , VkQueueFlags
  , vkGetPhysicalDeviceMemoryProperties
  , VkPhysicalDeviceMemoryProperties(..)
  , VkMemoryType(..)
  , VkMemoryPropertyFlags
  , VkMemoryHeap(..)
  , VkMemoryHeapFlags
  , vkGetInstanceProcAddr
  , vkGetDeviceProcAddr
  , VkDevice
  -- ** Device commands
  , vkCreateDevice
  , VkDeviceCreateInfo(..)
  , VkDeviceCreateFlags
  , VkDeviceQueueCreateInfo(..)
  , VkDeviceQueueCreateFlags
  , vkDestroyDevice
  -- ** Extension discovery commands
  , vkEnumerateInstanceExtensionProperties
  , VkExtensionProperties(..)
  , vkEnumerateDeviceExtensionProperties
  -- ** Layer discovery commands
  , vkEnumerateInstanceLayerProperties
  , VkLayerProperties(..)
  , vkEnumerateDeviceLayerProperties
  -- ** queue commands
  , vkGetDeviceQueue
  , VkQueue
  , vkQueueSubmit
  , VkSubmitInfo(..)
  , VkSemaphore
  , VkPipelineStageFlags
  , VkCommandBuffer
  , VkFence
  , vkQueueWaitIdle
  , vkDeviceWaitIdle
  -- ** Memory commands
  , vkAllocateMemory
  , VkMemoryAllocateInfo(..)
  , VkDeviceMemory
  , vkFreeMemory
  , vkMapMemory
  , VkMemoryMapFlags
  , vkUnmapMemory
  , vkFlushMappedMemoryRanges
  , VkMappedMemoryRange(..)
  , vkInvalidateMappedMemoryRanges
  , vkGetDeviceMemoryCommitment
  -- ** Memory management API commands
  , vkBindBufferMemory
  , VkBuffer
  , vkBindImageMemory
  , VkImage
  , vkGetBufferMemoryRequirements
  , VkMemoryRequirements(..)
  , vkGetImageMemoryRequirements
  -- ** Sparse resource memory management API commands
  , vkGetImageSparseMemoryRequirements
  , VkSparseImageMemoryRequirements(..)
  , VkSparseImageFormatProperties(..)
  , VkImageAspectFlags
  , VkSparseImageFormatFlags
  , vkGetPhysicalDeviceSparseImageFormatProperties
  , VkSampleCountFlagBits
  , pattern VK_SAMPLE_COUNT_1_BIT
  , pattern VK_SAMPLE_COUNT_2_BIT
  , pattern VK_SAMPLE_COUNT_4_BIT
  , pattern VK_SAMPLE_COUNT_8_BIT
  , pattern VK_SAMPLE_COUNT_16_BIT
  , pattern VK_SAMPLE_COUNT_32_BIT
  , pattern VK_SAMPLE_COUNT_64_BIT
  , vkQueueBindSparse
  , VkBindSparseInfo(..)
  , VkSparseBufferMemoryBindInfo(..)
  , VkSparseMemoryBind(..)
  , VkSparseMemoryBindFlags
  , VkSparseImageOpaqueMemoryBindInfo(..)
  , VkSparseImageMemoryBindInfo(..)
  , VkSparseImageMemoryBind(..)
  , VkImageSubresource(..)
  , VkOffset3D(..)
  -- ** Fence commands
  , vkCreateFence
  , VkFenceCreateInfo(..)
  , VkFenceCreateFlags
  , vkDestroyFence
  , vkResetFences
  , vkGetFenceStatus
  , vkWaitForFences
  -- ** Queue semaphore commands
  , vkCreateSemaphore
  , VkSemaphoreCreateInfo(..)
  , VkSemaphoreCreateFlags
  , vkDestroySemaphore
  -- ** Event commands
  , vkCreateEvent
  , VkEventCreateInfo(..)
  , VkEventCreateFlags
  , VkEvent
  , vkDestroyEvent
  , vkGetEventStatus
  , vkSetEvent
  , vkResetEvent
  -- ** Query commands
  , vkCreateQueryPool
  , VkQueryPoolCreateInfo(..)
  , VkQueryPoolCreateFlags
  , VkQueryType
  , pattern VK_QUERY_TYPE_OCCLUSION
  , pattern VK_QUERY_TYPE_PIPELINE_STATISTICS
  , pattern VK_QUERY_TYPE_TIMESTAMP
  , VkQueryPipelineStatisticFlags
  , VkQueryPool
  , vkDestroyQueryPool
  , vkGetQueryPoolResults
  , VkQueryResultFlags
  -- ** Buffer commands
  , vkCreateBuffer
  , VkBufferCreateInfo(..)
  , VkBufferCreateFlags
  , VkBufferUsageFlags
  , VkSharingMode
  , pattern VK_SHARING_MODE_EXCLUSIVE
  , pattern VK_SHARING_MODE_CONCURRENT
  , vkDestroyBuffer
  -- ** Buffer view commands
  , vkCreateBufferView
  , VkBufferViewCreateInfo(..)
  , VkBufferViewCreateFlags
  , VkBufferView
  , vkDestroyBufferView
  -- ** Image commands
  , vkCreateImage
  , VkImageCreateInfo(..)
  , VkImageLayout
  , pattern VK_IMAGE_LAYOUT_UNDEFINED
  , pattern VK_IMAGE_LAYOUT_GENERAL
  , pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_PREINITIALIZED
  , vkDestroyImage
  , vkGetImageSubresourceLayout
  , VkSubresourceLayout(..)
  -- ** Image view commands
  , vkCreateImageView
  , VkImageViewCreateInfo(..)
  , VkImageViewCreateFlags
  , VkImageViewType
  , pattern VK_IMAGE_VIEW_TYPE_1D
  , pattern VK_IMAGE_VIEW_TYPE_2D
  , pattern VK_IMAGE_VIEW_TYPE_3D
  , pattern VK_IMAGE_VIEW_TYPE_CUBE
  , pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY
  , pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
  , VkComponentMapping(..)
  , VkComponentSwizzle
  , pattern VK_COMPONENT_SWIZZLE_IDENTITY
  , pattern VK_COMPONENT_SWIZZLE_ZERO
  , pattern VK_COMPONENT_SWIZZLE_ONE
  , pattern VK_COMPONENT_SWIZZLE_R
  , pattern VK_COMPONENT_SWIZZLE_G
  , pattern VK_COMPONENT_SWIZZLE_B
  , pattern VK_COMPONENT_SWIZZLE_A
  , VkImageSubresourceRange(..)
  , VkImageView
  , vkDestroyImageView
  -- ** Shader commands
  , vkCreateShaderModule
  , VkShaderModuleCreateInfo(..)
  , VkShaderModuleCreateFlags
  , VkShaderModule
  , vkDestroyShaderModule
  -- ** Pipeline Cache commands
  , vkCreatePipelineCache
  , VkPipelineCacheCreateInfo(..)
  , VkPipelineCacheCreateFlags
  , VkPipelineCache
  , vkDestroyPipelineCache
  , vkGetPipelineCacheData
  , vkMergePipelineCaches
  -- ** Pipeline commands
  , vkCreateGraphicsPipelines
  , VkGraphicsPipelineCreateInfo(..)
  , VkPipelineCreateFlags
  , VkPipelineShaderStageCreateInfo(..)
  , VkPipelineShaderStageCreateFlags
  , VkShaderStageFlagBits
  , pattern VK_SHADER_STAGE_VERTEX_BIT
  , pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
  , pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
  , pattern VK_SHADER_STAGE_GEOMETRY_BIT
  , pattern VK_SHADER_STAGE_FRAGMENT_BIT
  , pattern VK_SHADER_STAGE_COMPUTE_BIT
  , pattern VK_SHADER_STAGE_ALL_GRAPHICS
  , pattern VK_SHADER_STAGE_ALL
  , VkSpecializationInfo(..)
  , VkSpecializationMapEntry(..)
  , VkPipelineVertexInputStateCreateInfo(..)
  , VkPipelineVertexInputStateCreateFlags
  , VkVertexInputBindingDescription(..)
  , VkVertexInputRate
  , pattern VK_VERTEX_INPUT_RATE_VERTEX
  , pattern VK_VERTEX_INPUT_RATE_INSTANCE
  , VkVertexInputAttributeDescription(..)
  , VkPipelineInputAssemblyStateCreateInfo(..)
  , VkPipelineInputAssemblyStateCreateFlags
  , VkPrimitiveTopology
  , pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
  , VkPipelineTessellationStateCreateInfo(..)
  , VkPipelineTessellationStateCreateFlags
  , VkPipelineViewportStateCreateInfo(..)
  , VkPipelineViewportStateCreateFlags
  , VkViewport(..)
  , VkRect2D(..)
  , VkOffset2D(..)
  , VkExtent2D(..)
  , VkPipelineRasterizationStateCreateInfo(..)
  , VkPipelineRasterizationStateCreateFlags
  , VkPolygonMode
  , pattern VK_POLYGON_MODE_FILL
  , pattern VK_POLYGON_MODE_LINE
  , pattern VK_POLYGON_MODE_POINT
  , VkCullModeFlags
  , VkFrontFace
  , pattern VK_FRONT_FACE_COUNTER_CLOCKWISE
  , pattern VK_FRONT_FACE_CLOCKWISE
  , VkPipelineMultisampleStateCreateInfo(..)
  , VkPipelineMultisampleStateCreateFlags
  , VkSampleMask
  , VkPipelineDepthStencilStateCreateInfo(..)
  , VkPipelineDepthStencilStateCreateFlags
  , VkCompareOp
  , pattern VK_COMPARE_OP_NEVER
  , pattern VK_COMPARE_OP_LESS
  , pattern VK_COMPARE_OP_EQUAL
  , pattern VK_COMPARE_OP_LESS_OR_EQUAL
  , pattern VK_COMPARE_OP_GREATER
  , pattern VK_COMPARE_OP_NOT_EQUAL
  , pattern VK_COMPARE_OP_GREATER_OR_EQUAL
  , pattern VK_COMPARE_OP_ALWAYS
  , VkStencilOpState(..)
  , VkStencilOp
  , pattern VK_STENCIL_OP_KEEP
  , pattern VK_STENCIL_OP_ZERO
  , pattern VK_STENCIL_OP_REPLACE
  , pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP
  , pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP
  , pattern VK_STENCIL_OP_INVERT
  , pattern VK_STENCIL_OP_INCREMENT_AND_WRAP
  , pattern VK_STENCIL_OP_DECREMENT_AND_WRAP
  , VkPipelineColorBlendStateCreateInfo(..)
  , VkPipelineColorBlendStateCreateFlags
  , VkLogicOp
  , pattern VK_LOGIC_OP_CLEAR
  , pattern VK_LOGIC_OP_AND
  , pattern VK_LOGIC_OP_AND_REVERSE
  , pattern VK_LOGIC_OP_COPY
  , pattern VK_LOGIC_OP_AND_INVERTED
  , pattern VK_LOGIC_OP_NO_OP
  , pattern VK_LOGIC_OP_XOR
  , pattern VK_LOGIC_OP_OR
  , pattern VK_LOGIC_OP_NOR
  , pattern VK_LOGIC_OP_EQUIVALENT
  , pattern VK_LOGIC_OP_INVERT
  , pattern VK_LOGIC_OP_OR_REVERSE
  , pattern VK_LOGIC_OP_COPY_INVERTED
  , pattern VK_LOGIC_OP_OR_INVERTED
  , pattern VK_LOGIC_OP_NAND
  , pattern VK_LOGIC_OP_SET
  , VkPipelineColorBlendAttachmentState(..)
  , VkBlendFactor
  , pattern VK_BLEND_FACTOR_ZERO
  , pattern VK_BLEND_FACTOR_ONE
  , pattern VK_BLEND_FACTOR_SRC_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR
  , pattern VK_BLEND_FACTOR_DST_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR
  , pattern VK_BLEND_FACTOR_SRC_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
  , pattern VK_BLEND_FACTOR_DST_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA
  , pattern VK_BLEND_FACTOR_CONSTANT_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR
  , pattern VK_BLEND_FACTOR_CONSTANT_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA
  , pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE
  , pattern VK_BLEND_FACTOR_SRC1_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR
  , pattern VK_BLEND_FACTOR_SRC1_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
  , VkBlendOp
  , pattern VK_BLEND_OP_ADD
  , pattern VK_BLEND_OP_SUBTRACT
  , pattern VK_BLEND_OP_REVERSE_SUBTRACT
  , pattern VK_BLEND_OP_MIN
  , pattern VK_BLEND_OP_MAX
  , VkColorComponentFlags
  , VkPipelineDynamicStateCreateInfo(..)
  , VkPipelineDynamicStateCreateFlags
  , VkDynamicState
  , pattern VK_DYNAMIC_STATE_VIEWPORT
  , pattern VK_DYNAMIC_STATE_SCISSOR
  , pattern VK_DYNAMIC_STATE_LINE_WIDTH
  , pattern VK_DYNAMIC_STATE_DEPTH_BIAS
  , pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS
  , pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS
  , pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK
  , pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK
  , pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE
  , VkPipelineLayout
  , VkRenderPass
  , VkPipeline
  , vkCreateComputePipelines
  , VkComputePipelineCreateInfo(..)
  , vkDestroyPipeline
  -- ** Pipeline layout commands
  , vkCreatePipelineLayout
  , VkPipelineLayoutCreateInfo(..)
  , VkPipelineLayoutCreateFlags
  , VkDescriptorSetLayout
  , VkPushConstantRange(..)
  , VkShaderStageFlags
  , vkDestroyPipelineLayout
  -- ** Sampler commands
  , vkCreateSampler
  , VkSamplerCreateInfo(..)
  , VkSamplerCreateFlags
  , VkFilter
  , pattern VK_FILTER_NEAREST
  , pattern VK_FILTER_LINEAR
  , VkSamplerMipmapMode
  , pattern VK_SAMPLER_MIPMAP_MODE_NEAREST
  , pattern VK_SAMPLER_MIPMAP_MODE_LINEAR
  , VkSamplerAddressMode
  , pattern VK_SAMPLER_ADDRESS_MODE_REPEAT
  , pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  , pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  , VkBorderColor
  , pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
  , pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK
  , pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE
  , pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE
  , VkSampler
  , vkDestroySampler
  -- ** Descriptor set commands
  , vkCreateDescriptorSetLayout
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkDescriptorSetLayoutCreateFlags
  , VkDescriptorSetLayoutBinding(..)
  , VkDescriptorType
  , pattern VK_DESCRIPTOR_TYPE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  , vkDestroyDescriptorSetLayout
  , vkCreateDescriptorPool
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorPoolCreateFlags
  , VkDescriptorPoolSize(..)
  , VkDescriptorPool
  , vkDestroyDescriptorPool
  , vkResetDescriptorPool
  , VkDescriptorPoolResetFlags
  , vkAllocateDescriptorSets
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSet
  , vkFreeDescriptorSets
  , vkUpdateDescriptorSets
  , VkWriteDescriptorSet(..)
  , VkDescriptorImageInfo(..)
  , VkDescriptorBufferInfo(..)
  , VkCopyDescriptorSet(..)
  -- ** Pass commands
  , vkCreateFramebuffer
  , VkFramebufferCreateInfo(..)
  , VkFramebufferCreateFlags
  , VkFramebuffer
  , vkDestroyFramebuffer
  , vkCreateRenderPass
  , VkRenderPassCreateInfo(..)
  , VkRenderPassCreateFlags
  , VkAttachmentDescription(..)
  , VkAttachmentDescriptionFlags
  , VkAttachmentLoadOp
  , pattern VK_ATTACHMENT_LOAD_OP_LOAD
  , pattern VK_ATTACHMENT_LOAD_OP_CLEAR
  , pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE
  , VkAttachmentStoreOp
  , pattern VK_ATTACHMENT_STORE_OP_STORE
  , pattern VK_ATTACHMENT_STORE_OP_DONT_CARE
  , VkSubpassDescription(..)
  , VkSubpassDescriptionFlags
  , VkPipelineBindPoint
  , pattern VK_PIPELINE_BIND_POINT_GRAPHICS
  , pattern VK_PIPELINE_BIND_POINT_COMPUTE
  , VkAttachmentReference(..)
  , VkSubpassDependency(..)
  , VkAccessFlags
  , VkDependencyFlags
  , vkDestroyRenderPass
  , vkGetRenderAreaGranularity
  -- ** Command pool commands
  , vkCreateCommandPool
  , VkCommandPoolCreateInfo(..)
  , VkCommandPoolCreateFlags
  , VkCommandPool
  , vkDestroyCommandPool
  , vkResetCommandPool
  , VkCommandPoolResetFlags
  -- ** Command buffer commands
  , vkAllocateCommandBuffers
  , VkCommandBufferAllocateInfo(..)
  , VkCommandBufferLevel
  , pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY
  , pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY
  , vkFreeCommandBuffers
  , vkBeginCommandBuffer
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferUsageFlags
  , VkCommandBufferInheritanceInfo(..)
  , VkQueryControlFlags
  , vkEndCommandBuffer
  , vkResetCommandBuffer
  , VkCommandBufferResetFlags
  -- ** Command buffer building commands
  , vkCmdBindPipeline
  , vkCmdSetViewport
  , vkCmdSetScissor
  , vkCmdSetLineWidth
  , vkCmdSetDepthBias
  , vkCmdSetBlendConstants
  , vkCmdSetDepthBounds
  , vkCmdSetStencilCompareMask
  , VkStencilFaceFlags
  , vkCmdSetStencilWriteMask
  , vkCmdSetStencilReference
  , vkCmdBindDescriptorSets
  , vkCmdBindIndexBuffer
  , VkIndexType
  , pattern VK_INDEX_TYPE_UINT16
  , pattern VK_INDEX_TYPE_UINT32
  , vkCmdBindVertexBuffers
  , vkCmdDraw
  , vkCmdDrawIndexed
  , vkCmdDrawIndirect
  , vkCmdDrawIndexedIndirect
  , vkCmdDispatch
  , vkCmdDispatchIndirect
  , vkCmdCopyBuffer
  , VkBufferCopy(..)
  , vkCmdCopyImage
  , VkImageCopy(..)
  , VkImageSubresourceLayers(..)
  , vkCmdBlitImage
  , VkImageBlit(..)
  , vkCmdCopyBufferToImage
  , VkBufferImageCopy(..)
  , vkCmdCopyImageToBuffer
  , vkCmdUpdateBuffer
  , vkCmdFillBuffer
  , vkCmdClearColorImage
  , VkClearColorValue(..)
  , vkCmdClearDepthStencilImage
  , VkClearDepthStencilValue(..)
  , vkCmdClearAttachments
  , VkClearAttachment(..)
  , VkClearValue(..)
  , VkClearRect(..)
  , vkCmdResolveImage
  , VkImageResolve(..)
  , vkCmdSetEvent
  , vkCmdResetEvent
  , vkCmdWaitEvents
  , VkMemoryBarrier(..)
  , VkBufferMemoryBarrier(..)
  , VkImageMemoryBarrier(..)
  , vkCmdPipelineBarrier
  , vkCmdBeginQuery
  , vkCmdEndQuery
  , vkCmdResetQueryPool
  , vkCmdWriteTimestamp
  , VkPipelineStageFlagBits
  , pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT
  , pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT
  , pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
  , pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
  , pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
  , pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TRANSFER_BIT
  , pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
  , pattern VK_PIPELINE_STAGE_HOST_BIT
  , pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT
  , pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
  , vkCmdCopyQueryPoolResults
  , vkCmdPushConstants
  , vkCmdBeginRenderPass
  , VkRenderPassBeginInfo(..)
  , VkSubpassContents
  , pattern VK_SUBPASS_CONTENTS_INLINE
  , pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
  , vkCmdNextSubpass
  , vkCmdEndRenderPass
  , vkCmdExecuteCommands
  -- ** Types not directly used by the API
  , VkDispatchIndirectCommand(..)
  , VkDrawIndexedIndirectCommand(..)
  , VkDrawIndirectCommand(..)
  -- * Extensions
  
  -- ** VK_KHR_surface
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Nothing

  , pattern VK_KHR_SURFACE_SPEC_VERSION
  , pattern VK_KHR_SURFACE_EXTENSION_NAME
  , pattern VK_ERROR_SURFACE_LOST_KHR
  , pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  , vkDestroySurfaceKHR
  , VkSurfaceKHR
  , vkGetPhysicalDeviceSurfaceSupportKHR
  , vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceTransformFlagsKHR
  , VkSurfaceTransformFlagBitsKHR
  , pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR
  , VkCompositeAlphaFlagsKHR
  , vkGetPhysicalDeviceSurfaceFormatsKHR
  , VkSurfaceFormatKHR(..)
  , VkColorSpaceKHR
  , pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR
  , vkGetPhysicalDeviceSurfacePresentModesKHR
  , VkPresentModeKHR
  , pattern VK_PRESENT_MODE_IMMEDIATE_KHR
  , pattern VK_PRESENT_MODE_MAILBOX_KHR
  , pattern VK_PRESENT_MODE_FIFO_KHR
  , pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR
  
  -- ** VK_KHR_swapchain
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Nothing

  , pattern VK_KHR_SWAPCHAIN_SPEC_VERSION
  , pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
  , pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  , pattern VK_SUBOPTIMAL_KHR
  , pattern VK_ERROR_OUT_OF_DATE_KHR
  , vkCreateSwapchainKHR
  , VkSwapchainCreateInfoKHR(..)
  , VkSwapchainCreateFlagsKHR
  , VkCompositeAlphaFlagBitsKHR
  , pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
  , VkSwapchainKHR
  , vkDestroySwapchainKHR
  , vkGetSwapchainImagesKHR
  , vkAcquireNextImageKHR
  , vkQueuePresentKHR
  , VkPresentInfoKHR(..)
  , VkResult
  , pattern VK_SUCCESS
  , pattern VK_NOT_READY
  , pattern VK_TIMEOUT
  , pattern VK_EVENT_SET
  , pattern VK_EVENT_RESET
  , pattern VK_INCOMPLETE
  , pattern VK_ERROR_OUT_OF_HOST_MEMORY
  , pattern VK_ERROR_OUT_OF_DEVICE_MEMORY
  , pattern VK_ERROR_INITIALIZATION_FAILED
  , pattern VK_ERROR_DEVICE_LOST
  , pattern VK_ERROR_MEMORY_MAP_FAILED
  , pattern VK_ERROR_LAYER_NOT_PRESENT
  , pattern VK_ERROR_EXTENSION_NOT_PRESENT
  , pattern VK_ERROR_FEATURE_NOT_PRESENT
  , pattern VK_ERROR_INCOMPATIBLE_DRIVER
  , pattern VK_ERROR_TOO_MANY_OBJECTS
  , pattern VK_ERROR_FORMAT_NOT_SUPPORTED
  
  -- ** VK_KHR_display
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Nothing

  , VkDisplayPlaneAlphaFlagsKHR
  , VkDisplayPlaneAlphaFlagBitsKHR
  , pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
  , VkDisplayPropertiesKHR(..)
  , VkDisplayKHR
  , VkDisplayModeParametersKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayModeKHR
  , VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModeCreateFlagsKHR
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , VkDisplaySurfaceCreateFlagsKHR
  , pattern VK_KHR_DISPLAY_SPEC_VERSION
  , pattern VK_KHR_DISPLAY_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  , vkGetPhysicalDeviceDisplayPropertiesKHR
  , vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , vkGetDisplayPlaneSupportedDisplaysKHR
  , vkGetDisplayModePropertiesKHR
  , vkCreateDisplayModeKHR
  , vkGetDisplayPlaneCapabilitiesKHR
  , vkCreateDisplayPlaneSurfaceKHR
  
  -- ** VK_KHR_display_swapchain
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Nothing

  , VkDisplayPresentInfoKHR(..)
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  , pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
  , vkCreateSharedSwapchainsKHR
  
  -- ** VK_KHR_xlib_surface
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Just "VK_USE_PLATFORM_XLIB_KHR"

  , VkXlibSurfaceCreateFlagsKHR
  , VkXlibSurfaceCreateInfoKHR(..)
  , pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION
  , pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
  , vkCreateXlibSurfaceKHR
  , vkGetPhysicalDeviceXlibPresentationSupportKHR
  
  -- ** VK_KHR_xcb_surface
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Just "VK_USE_PLATFORM_XCB_KHR"

  , VkXcbSurfaceCreateFlagsKHR
  , VkXcbSurfaceCreateInfoKHR(..)
  , pattern VK_KHR_XCB_SURFACE_SPEC_VERSION
  , pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
  , vkCreateXcbSurfaceKHR
  , vkGetPhysicalDeviceXcbPresentationSupportKHR
  
  -- ** VK_KHR_wayland_surface
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Just "VK_USE_PLATFORM_WAYLAND_KHR"

  , VkWaylandSurfaceCreateFlagsKHR
  , VkWaylandSurfaceCreateInfoKHR(..)
  , pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION
  , pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  , vkCreateWaylandSurfaceKHR
  , vkGetPhysicalDeviceWaylandPresentationSupportKHR
  
  -- ** VK_KHR_mir_surface
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Just "VK_USE_PLATFORM_MIR_KHR"

  , VkMirSurfaceCreateFlagsKHR
  , VkMirSurfaceCreateInfoKHR(..)
  , pattern VK_KHR_MIR_SURFACE_SPEC_VERSION
  , pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
  , vkCreateMirSurfaceKHR
  , vkGetPhysicalDeviceMirPresentationSupportKHR
  
  -- ** VK_KHR_android_surface
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Just "VK_USE_PLATFORM_ANDROID_KHR"

  , VkAndroidSurfaceCreateFlagsKHR
  , VkAndroidSurfaceCreateInfoKHR(..)
  , pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION
  , pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
  , vkCreateAndroidSurfaceKHR
  
  -- ** VK_KHR_win32_surface
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: vulkan
  -- Protect: Just "VK_USE_PLATFORM_WIN32_KHR"

  , VkWin32SurfaceCreateFlagsKHR
  , VkWin32SurfaceCreateInfoKHR(..)
  , pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION
  , pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
  , vkCreateWin32SurfaceKHR
  , vkGetPhysicalDeviceWin32PresentationSupportKHR
  
  -- ** VK_ANDROID_native_buffer
  -- Author: Nothing
  -- Contact: Nothing
  -- Support: disabled
  -- Protect: Nothing

  , pattern VK_ANDROID_NATIVE_BUFFER_SPEC_VERSION
  , pattern VK_ANDROID_NATIVE_BUFFER_NUMBER
  , pattern VK_ANDROID_NATIVE_BUFFER_NAME
  
  -- ** VK_EXT_debug_report
  -- Author: Just "Google, Inc."
  -- Contact: Just "Courtney Goeltzenleuchter @courtney"
  -- Support: vulkan
  -- Protect: Nothing

  , VkDebugReportObjectTypeEXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT
  , VkDebugReportErrorEXT
  , pattern VK_DEBUG_REPORT_ERROR_NONE_EXT
  , pattern VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT
  , pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION
  , pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
  , pattern VK_ERROR_VALIDATION_FAILED_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT
  , vkCreateDebugReportCallbackEXT
  , VkDebugReportCallbackCreateInfoEXT(..)
  , VkDebugReportFlagsEXT
  , VkDebugReportCallbackEXT
  , vkDestroyDebugReportCallbackEXT
  , vkDebugReportMessageEXT
  
  -- ** VK_NV_glsl_shader
  -- Author: Just "NVIDIA"
  -- Contact: Just "Piers Daniell @pdaniell"
  -- Support: vulkan
  -- Protect: Nothing

  , pattern VK_NV_GLSL_SHADER_SPEC_VERSION
  , pattern VK_NV_GLSL_SHADER_EXTENSION_NAME
  , pattern VK_ERROR_INVALID_SHADER_NV
  
  -- ** VK_NV_extension_1
  -- Author: Just "NVIDIA"
  -- Contact: Just "Piers Daniell @pdaniell"
  -- Support: disabled
  -- Protect: Nothing

  , pattern VK_NV_EXTENSION_1_SPEC_VERSION
  , pattern VK_NV_EXTENSION_1_EXTENSION_NAME
  , pattern VK_NV_EXTENSION_1_ERROR
  
  -- ** VK_KHR_sampler_mirror_clamp_to_edge
  -- Author: Just "KHR"
  -- Contact: Just "Tobias Hector @tobias"
  -- Support: vulkan
  -- Protect: Nothing

  , pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION
  , pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
  , pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
  
  -- ** VK_IMG_filter_cubic
  -- Author: Just "IMG"
  -- Contact: Just "Tobias Hector @tobias"
  -- Support: vulkan
  -- Protect: Nothing

  , pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION
  , pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME
  , pattern VK_FILTER_CUBIC_IMG
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  
  -- ** VK_AMD_extension_1
  -- Author: Just "AMD"
  -- Contact: Just "Daniel Rakos @aqnuep"
  -- Support: disabled
  -- Protect: Nothing

  , pattern VK_AMD_EXTENSION_1_SPEC_VERSION
  , pattern VK_AMD_EXTENSION_1_EXTENSION_NAME
  
  -- ** VK_AMD_extension_2
  -- Author: Just "AMD"
  -- Contact: Just "Daniel Rakos @aqnuep"
  -- Support: disabled
  -- Protect: Nothing

  , pattern VK_AMD_EXTENSION_2_SPEC_VERSION
  , pattern VK_AMD_EXTENSION_2_EXTENSION_NAME
  
  -- ** VK_AMD_rasterization_order
  -- Author: Just "AMD"
  -- Contact: Just "Daniel Rakos @aqnuep"
  -- Support: vulkan
  -- Protect: Nothing

  , VkRasterizationOrderAMD
  , pattern VK_RASTERIZATION_ORDER_STRICT_AMD
  , pattern VK_RASTERIZATION_ORDER_RELAXED_AMD
  , VkPipelineRasterizationStateRasterizationOrderAMD(..)
  , pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION
  , pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
  
  -- ** VK_AMD_extension_4
  -- Author: Just "AMD"
  -- Contact: Just "Daniel Rakos @aqnuep"
  -- Support: disabled
  -- Protect: Nothing

  , pattern VK_AMD_EXTENSION_4_SPEC_VERSION
  , pattern VK_AMD_EXTENSION_4_EXTENSION_NAME
  
  -- ** VK_AMD_extension_5
  -- Author: Just "AMD"
  -- Contact: Just "Daniel Rakos @aqnuep"
  -- Support: disabled
  -- Protect: Nothing

  , pattern VK_AMD_EXTENSION_5_SPEC_VERSION
  , pattern VK_AMD_EXTENSION_5_EXTENSION_NAME
  
  -- ** VK_AMD_extension_6
  -- Author: Just "AMD"
  -- Contact: Just "Daniel Rakos @aqnuep"
  -- Support: disabled
  -- Protect: Nothing

  , pattern VK_AMD_EXTENSION_6_SPEC_VERSION
  , pattern VK_AMD_EXTENSION_6_EXTENSION_NAME
  
  -- ** VK_EXT_debug_marker
  -- Author: Just "Baldur Karlsson"
  -- Contact: Just "baldurk@baldurk.org"
  -- Support: vulkan
  -- Protect: Nothing

  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  , VkDebugMarkerMarkerInfoEXT(..)
  , pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION
  , pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  , vkDebugMarkerSetObjectTagEXT
  , vkDebugMarkerSetObjectNameEXT
  , vkCmdDebugMarkerBeginEXT
  , vkCmdDebugMarkerEndEXT
  , vkCmdDebugMarkerInsertEXT
) where
import Foreign
import Foreign.C
import Data.Bits
import Data.Int
import Data.Word
import Data.Maybe (fromJust)
import Data.Vector (fromListN)
import Linear
import Linear.V
import Numeric.Half
import Numeric.Fixed
import Control.Monad.IO.Class
import System.IO.Unsafe (unsafePerformIO)

-- | This function is only safe on the first 256 characters.
castToFixedString :: Dim n => String -> V n CChar
castToFixedString x = result
  where d = dim result
        result = fromJust . fromVector . fromListN d $
                   map castCharToCChar (x ++ replicate (d - length x) '\0')

foreign import ccall unsafe "&vkGetInstanceProcAddr" fp_vkGetInstanceProcAddr :: FunPtr (VkInstance -> Ptr CChar -> IO (FunPtr a))

fp_vkCreateInstance :: FunPtr (Ptr VkInstanceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkInstance -> IO VkResult)
fp_vkCreateInstance = castFunPtr $ unsafePerformIO (getInstanceProcAddr nullPtr "vkCreateInstance")

fp_vkEnumerateInstanceLayerProperties :: FunPtr (Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult)
fp_vkEnumerateInstanceLayerProperties = castFunPtr $ unsafePerformIO (getInstanceProcAddr nullPtr "vkEnumerateInstanceLayerProperties")

fp_vkEnumerateInstanceExtensionProperties :: FunPtr (Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult)
fp_vkEnumerateInstanceExtensionProperties = castFunPtr $ unsafePerformIO (getInstanceProcAddr nullPtr "vkEnumerateInstanceExtensionProperties")

getInstanceProcAddr :: VkInstance -> String -> IO (FunPtr a)
getInstanceProcAddr vulkan proc =
        return . castFunPtr =<< withCAString proc (vkGetInstanceProcAddr vulkan)

pattern VK_HEADER_VERSION = 12
pattern VK_LOD_CLAMP_NONE = 1000 :: Float
pattern VK_REMAINING_MIP_LEVELS = 0xffffffff :: Word32
pattern VK_REMAINING_ARRAY_LAYERS = 0xffffffff :: Word32
pattern VK_WHOLE_SIZE = 0xffffffffffffffff :: Word64
pattern VK_ATTACHMENT_UNUSED = 0xffffffff :: Word32
pattern VK_TRUE = 1 :: VkBool32
pattern VK_FALSE = 0 :: VkBool32
pattern VK_QUEUE_FAMILY_IGNORED = 0xffffffff :: Word32
pattern VK_SUBPASS_EXTERNAL = 0xffffffff :: Word32

-- X11/Xlib.h
type Display = Ptr ()
type VisualID = Ptr ()
type Window = Ptr ()

-- android/native_window.h
type ANativeWindow = Ptr ()

-- mir_toolkit/client_types.h
type MirConnection = Ptr ()
type MirSurface = Ptr ()

-- wayland-client.h
type WlDisplay = Ptr () -- ^ wl_display
type WlSurface = Ptr () -- ^ wl_surface

-- windows.h
type HINSTANCE = Ptr ()
type HWND = Ptr ()

-- xcb/xcb.h
type XcbConnection = Ptr () -- ^ xcb_connection_t
type XcbVisualId = Ptr () -- ^ xcb_visualid_t
type XcbWindow = Ptr () -- ^ xcb_window_t


data VulkanSetup = VulkanSetup
  { fp_vkDestroyInstance :: !(FunPtr (VkInstance -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkEnumeratePhysicalDevices :: !(FunPtr (VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO VkResult))
  , fp_vkGetDeviceProcAddr :: !(FunPtr (VkDevice -> Ptr CChar -> IO PFN_vkVoidFunction))
  , fp_vkGetPhysicalDeviceProperties :: !(FunPtr (VkPhysicalDevice -> Ptr VkPhysicalDeviceProperties -> IO ()))
  , fp_vkGetPhysicalDeviceQueueFamilyProperties :: !(FunPtr (VkPhysicalDevice -> Ptr Word32 -> Ptr VkQueueFamilyProperties -> IO ()))
  , fp_vkGetPhysicalDeviceMemoryProperties :: !(FunPtr (VkPhysicalDevice -> Ptr VkPhysicalDeviceMemoryProperties -> IO ()))
  , fp_vkGetPhysicalDeviceFeatures :: !(FunPtr (VkPhysicalDevice -> Ptr VkPhysicalDeviceFeatures -> IO ()))
  , fp_vkGetPhysicalDeviceFormatProperties :: !(FunPtr (VkPhysicalDevice -> VkFormat -> Ptr VkFormatProperties -> IO ()))
  , fp_vkGetPhysicalDeviceImageFormatProperties :: !(FunPtr (VkPhysicalDevice -> VkFormat -> VkImageType -> VkImageTiling -> VkImageUsageFlags -> VkImageCreateFlags -> Ptr VkImageFormatProperties -> IO VkResult))
  , fp_vkCreateDevice :: !(FunPtr (VkPhysicalDevice -> Ptr VkDeviceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDevice -> IO VkResult))
  , fp_vkEnumerateDeviceLayerProperties :: !(FunPtr (VkPhysicalDevice -> Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult))
  , fp_vkEnumerateDeviceExtensionProperties :: !(FunPtr (VkPhysicalDevice -> Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult))
  , fp_vkGetPhysicalDeviceSparseImageFormatProperties :: !(FunPtr (VkPhysicalDevice -> VkFormat -> VkImageType -> VkSampleCountFlagBits -> VkImageUsageFlags -> VkImageTiling -> Ptr Word32 -> Ptr VkSparseImageFormatProperties -> IO ()))
  , fp_vkCreateAndroidSurfaceKHR :: !(FunPtr (VkInstance -> Ptr VkAndroidSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult))
  , fp_vkGetPhysicalDeviceDisplayPropertiesKHR :: !(FunPtr (VkPhysicalDevice -> Ptr Word32 -> Ptr VkDisplayPropertiesKHR -> IO VkResult))
  , fp_vkGetPhysicalDeviceDisplayPlanePropertiesKHR :: !(FunPtr (VkPhysicalDevice -> Ptr Word32 -> Ptr VkDisplayPlanePropertiesKHR -> IO VkResult))
  , fp_vkGetDisplayPlaneSupportedDisplaysKHR :: !(FunPtr (VkPhysicalDevice -> Word32 -> Ptr Word32 -> Ptr VkDisplayKHR -> IO VkResult))
  , fp_vkGetDisplayModePropertiesKHR :: !(FunPtr (VkPhysicalDevice -> VkDisplayKHR -> Ptr Word32 -> Ptr VkDisplayModePropertiesKHR -> IO VkResult))
  , fp_vkCreateDisplayModeKHR :: !(FunPtr (VkPhysicalDevice -> VkDisplayKHR -> Ptr VkDisplayModeCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkDisplayModeKHR -> IO VkResult))
  , fp_vkGetDisplayPlaneCapabilitiesKHR :: !(FunPtr (VkPhysicalDevice -> VkDisplayModeKHR -> Word32 -> Ptr VkDisplayPlaneCapabilitiesKHR -> IO VkResult))
  , fp_vkCreateDisplayPlaneSurfaceKHR :: !(FunPtr (VkInstance -> Ptr VkDisplaySurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult))
  , fp_vkCreateMirSurfaceKHR :: !(FunPtr (VkInstance -> Ptr VkMirSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult))
  , fp_vkGetPhysicalDeviceMirPresentationSupportKHR :: !(FunPtr (VkPhysicalDevice -> Word32 -> Ptr MirConnection -> IO VkBool32))
  , fp_vkDestroySurfaceKHR :: !(FunPtr (VkInstance -> VkSurfaceKHR -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkGetPhysicalDeviceSurfaceSupportKHR :: !(FunPtr (VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> Ptr VkBool32 -> IO VkResult))
  , fp_vkGetPhysicalDeviceSurfaceCapabilitiesKHR :: !(FunPtr (VkPhysicalDevice -> VkSurfaceKHR -> Ptr VkSurfaceCapabilitiesKHR -> IO VkResult))
  , fp_vkGetPhysicalDeviceSurfaceFormatsKHR :: !(FunPtr (VkPhysicalDevice -> VkSurfaceKHR -> Ptr Word32 -> Ptr VkSurfaceFormatKHR -> IO VkResult))
  , fp_vkGetPhysicalDeviceSurfacePresentModesKHR :: !(FunPtr (VkPhysicalDevice -> VkSurfaceKHR -> Ptr Word32 -> Ptr VkPresentModeKHR -> IO VkResult))
  , fp_vkCreateWaylandSurfaceKHR :: !(FunPtr (VkInstance -> Ptr VkWaylandSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult))
  , fp_vkGetPhysicalDeviceWaylandPresentationSupportKHR :: !(FunPtr (VkPhysicalDevice -> Word32 -> Ptr WlDisplay -> IO VkBool32))
  , fp_vkCreateWin32SurfaceKHR :: !(FunPtr (VkInstance -> Ptr VkWin32SurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult))
  , fp_vkGetPhysicalDeviceWin32PresentationSupportKHR :: !(FunPtr (VkPhysicalDevice -> Word32 -> IO VkBool32))
  , fp_vkCreateXlibSurfaceKHR :: !(FunPtr (VkInstance -> Ptr VkXlibSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult))
  , fp_vkGetPhysicalDeviceXlibPresentationSupportKHR :: !(FunPtr (VkPhysicalDevice -> Word32 -> Ptr Display -> VisualID -> IO VkBool32))
  , fp_vkCreateXcbSurfaceKHR :: !(FunPtr (VkInstance -> Ptr VkXcbSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult))
  , fp_vkGetPhysicalDeviceXcbPresentationSupportKHR :: !(FunPtr (VkPhysicalDevice -> Word32 -> Ptr XcbConnection -> XcbVisualId -> IO VkBool32))
  , fp_vkCreateDebugReportCallbackEXT :: !(FunPtr (VkInstance -> Ptr VkDebugReportCallbackCreateInfoEXT -> Ptr VkAllocationCallbacks -> Ptr VkDebugReportCallbackEXT -> IO VkResult))
  , fp_vkDestroyDebugReportCallbackEXT :: !(FunPtr (VkInstance -> VkDebugReportCallbackEXT -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkDebugReportMessageEXT :: !(FunPtr (VkInstance -> VkDebugReportFlagsEXT -> VkDebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ()))
  }

getVulkanSetup :: MonadIO m => VkInstance -> m VulkanSetup
getVulkanSetup vulkan = liftIO $ return VulkanSetup
  <*> getInstanceProcAddr vulkan "vkDestroyInstance"
    <*> getInstanceProcAddr vulkan "vkEnumeratePhysicalDevices"
    <*> getInstanceProcAddr vulkan "vkGetDeviceProcAddr"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceProperties"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceQueueFamilyProperties"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceMemoryProperties"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceFeatures"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceFormatProperties"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceImageFormatProperties"
    <*> getInstanceProcAddr vulkan "vkCreateDevice"
    <*> getInstanceProcAddr vulkan "vkEnumerateDeviceLayerProperties"
    <*> getInstanceProcAddr vulkan "vkEnumerateDeviceExtensionProperties"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceSparseImageFormatProperties"
    <*> getInstanceProcAddr vulkan "vkCreateAndroidSurfaceKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceDisplayPropertiesKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
    <*> getInstanceProcAddr vulkan "vkGetDisplayPlaneSupportedDisplaysKHR"
    <*> getInstanceProcAddr vulkan "vkGetDisplayModePropertiesKHR"
    <*> getInstanceProcAddr vulkan "vkCreateDisplayModeKHR"
    <*> getInstanceProcAddr vulkan "vkGetDisplayPlaneCapabilitiesKHR"
    <*> getInstanceProcAddr vulkan "vkCreateDisplayPlaneSurfaceKHR"
    <*> getInstanceProcAddr vulkan "vkCreateMirSurfaceKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceMirPresentationSupportKHR"
    <*> getInstanceProcAddr vulkan "vkDestroySurfaceKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceSurfaceSupportKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceSurfaceFormatsKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceSurfacePresentModesKHR"
    <*> getInstanceProcAddr vulkan "vkCreateWaylandSurfaceKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
    <*> getInstanceProcAddr vulkan "vkCreateWin32SurfaceKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceWin32PresentationSupportKHR"
    <*> getInstanceProcAddr vulkan "vkCreateXlibSurfaceKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceXlibPresentationSupportKHR"
    <*> getInstanceProcAddr vulkan "vkCreateXcbSurfaceKHR"
    <*> getInstanceProcAddr vulkan "vkGetPhysicalDeviceXcbPresentationSupportKHR"
    <*> getInstanceProcAddr vulkan "vkCreateDebugReportCallbackEXT"
    <*> getInstanceProcAddr vulkan "vkDestroyDebugReportCallbackEXT"
    <*> getInstanceProcAddr vulkan "vkDebugReportMessageEXT"

data Vulkan = Vulkan
  { fp_vkDestroyDevice :: !(FunPtr (VkDevice -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkGetDeviceQueue :: !(FunPtr (VkDevice -> Word32 -> Word32 -> Ptr VkQueue -> IO ()))
  , fp_vkQueueSubmit :: !(FunPtr (VkQueue -> Word32 -> Ptr VkSubmitInfo -> VkFence -> IO VkResult))
  , fp_vkQueueWaitIdle :: !(FunPtr (VkQueue -> IO VkResult))
  , fp_vkDeviceWaitIdle :: !(FunPtr (VkDevice -> IO VkResult))
  , fp_vkAllocateMemory :: !(FunPtr (VkDevice -> Ptr VkMemoryAllocateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDeviceMemory -> IO VkResult))
  , fp_vkFreeMemory :: !(FunPtr (VkDevice -> VkDeviceMemory -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkMapMemory :: !(FunPtr (VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags -> Ptr (Ptr ()) -> IO VkResult))
  , fp_vkUnmapMemory :: !(FunPtr (VkDevice -> VkDeviceMemory -> IO ()))
  , fp_vkFlushMappedMemoryRanges :: !(FunPtr (VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult))
  , fp_vkInvalidateMappedMemoryRanges :: !(FunPtr (VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult))
  , fp_vkGetDeviceMemoryCommitment :: !(FunPtr (VkDevice -> VkDeviceMemory -> Ptr VkDeviceSize -> IO ()))
  , fp_vkGetBufferMemoryRequirements :: !(FunPtr (VkDevice -> VkBuffer -> Ptr VkMemoryRequirements -> IO ()))
  , fp_vkBindBufferMemory :: !(FunPtr (VkDevice -> VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO VkResult))
  , fp_vkGetImageMemoryRequirements :: !(FunPtr (VkDevice -> VkImage -> Ptr VkMemoryRequirements -> IO ()))
  , fp_vkBindImageMemory :: !(FunPtr (VkDevice -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO VkResult))
  , fp_vkGetImageSparseMemoryRequirements :: !(FunPtr (VkDevice -> VkImage -> Ptr Word32 -> Ptr VkSparseImageMemoryRequirements -> IO ()))
  , fp_vkQueueBindSparse :: !(FunPtr (VkQueue -> Word32 -> Ptr VkBindSparseInfo -> VkFence -> IO VkResult))
  , fp_vkCreateFence :: !(FunPtr (VkDevice -> Ptr VkFenceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult))
  , fp_vkDestroyFence :: !(FunPtr (VkDevice -> VkFence -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkResetFences :: !(FunPtr (VkDevice -> Word32 -> Ptr VkFence -> IO VkResult))
  , fp_vkGetFenceStatus :: !(FunPtr (VkDevice -> VkFence -> IO VkResult))
  , fp_vkWaitForFences :: !(FunPtr (VkDevice -> Word32 -> Ptr VkFence -> VkBool32 -> Word64 -> IO VkResult))
  , fp_vkCreateSemaphore :: !(FunPtr (VkDevice -> Ptr VkSemaphoreCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkSemaphore -> IO VkResult))
  , fp_vkDestroySemaphore :: !(FunPtr (VkDevice -> VkSemaphore -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreateEvent :: !(FunPtr (VkDevice -> Ptr VkEventCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkEvent -> IO VkResult))
  , fp_vkDestroyEvent :: !(FunPtr (VkDevice -> VkEvent -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkGetEventStatus :: !(FunPtr (VkDevice -> VkEvent -> IO VkResult))
  , fp_vkSetEvent :: !(FunPtr (VkDevice -> VkEvent -> IO VkResult))
  , fp_vkResetEvent :: !(FunPtr (VkDevice -> VkEvent -> IO VkResult))
  , fp_vkCreateQueryPool :: !(FunPtr (VkDevice -> Ptr VkQueryPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkQueryPool -> IO VkResult))
  , fp_vkDestroyQueryPool :: !(FunPtr (VkDevice -> VkQueryPool -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkGetQueryPoolResults :: !(FunPtr (VkDevice -> VkQueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> VkDeviceSize -> VkQueryResultFlags -> IO VkResult))
  , fp_vkCreateBuffer :: !(FunPtr (VkDevice -> Ptr VkBufferCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkBuffer -> IO VkResult))
  , fp_vkDestroyBuffer :: !(FunPtr (VkDevice -> VkBuffer -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreateBufferView :: !(FunPtr (VkDevice -> Ptr VkBufferViewCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkBufferView -> IO VkResult))
  , fp_vkDestroyBufferView :: !(FunPtr (VkDevice -> VkBufferView -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreateImage :: !(FunPtr (VkDevice -> Ptr VkImageCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImage -> IO VkResult))
  , fp_vkDestroyImage :: !(FunPtr (VkDevice -> VkImage -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkGetImageSubresourceLayout :: !(FunPtr (VkDevice -> VkImage -> Ptr VkImageSubresource -> Ptr VkSubresourceLayout -> IO ()))
  , fp_vkCreateImageView :: !(FunPtr (VkDevice -> Ptr VkImageViewCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImageView -> IO VkResult))
  , fp_vkDestroyImageView :: !(FunPtr (VkDevice -> VkImageView -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreateShaderModule :: !(FunPtr (VkDevice -> Ptr VkShaderModuleCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkShaderModule -> IO VkResult))
  , fp_vkDestroyShaderModule :: !(FunPtr (VkDevice -> VkShaderModule -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreatePipelineCache :: !(FunPtr (VkDevice -> Ptr VkPipelineCacheCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipelineCache -> IO VkResult))
  , fp_vkDestroyPipelineCache :: !(FunPtr (VkDevice -> VkPipelineCache -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkGetPipelineCacheData :: !(FunPtr (VkDevice -> VkPipelineCache -> Ptr CSize -> Ptr () -> IO VkResult))
  , fp_vkMergePipelineCaches :: !(FunPtr (VkDevice -> VkPipelineCache -> Word32 -> Ptr VkPipelineCache -> IO VkResult))
  , fp_vkCreateGraphicsPipelines :: !(FunPtr (VkDevice -> VkPipelineCache -> Word32 -> Ptr VkGraphicsPipelineCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult))
  , fp_vkCreateComputePipelines :: !(FunPtr (VkDevice -> VkPipelineCache -> Word32 -> Ptr VkComputePipelineCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult))
  , fp_vkDestroyPipeline :: !(FunPtr (VkDevice -> VkPipeline -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreatePipelineLayout :: !(FunPtr (VkDevice -> Ptr VkPipelineLayoutCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipelineLayout -> IO VkResult))
  , fp_vkDestroyPipelineLayout :: !(FunPtr (VkDevice -> VkPipelineLayout -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreateSampler :: !(FunPtr (VkDevice -> Ptr VkSamplerCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkSampler -> IO VkResult))
  , fp_vkDestroySampler :: !(FunPtr (VkDevice -> VkSampler -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreateDescriptorSetLayout :: !(FunPtr (VkDevice -> Ptr VkDescriptorSetLayoutCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDescriptorSetLayout -> IO VkResult))
  , fp_vkDestroyDescriptorSetLayout :: !(FunPtr (VkDevice -> VkDescriptorSetLayout -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreateDescriptorPool :: !(FunPtr (VkDevice -> Ptr VkDescriptorPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDescriptorPool -> IO VkResult))
  , fp_vkDestroyDescriptorPool :: !(FunPtr (VkDevice -> VkDescriptorPool -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkResetDescriptorPool :: !(FunPtr (VkDevice -> VkDescriptorPool -> VkDescriptorPoolResetFlags -> IO VkResult))
  , fp_vkAllocateDescriptorSets :: !(FunPtr (VkDevice -> Ptr VkDescriptorSetAllocateInfo -> Ptr VkDescriptorSet -> IO VkResult))
  , fp_vkFreeDescriptorSets :: !(FunPtr (VkDevice -> VkDescriptorPool -> Word32 -> Ptr VkDescriptorSet -> IO VkResult))
  , fp_vkUpdateDescriptorSets :: !(FunPtr (VkDevice -> Word32 -> Ptr VkWriteDescriptorSet -> Word32 -> Ptr VkCopyDescriptorSet -> IO ()))
  , fp_vkCreateFramebuffer :: !(FunPtr (VkDevice -> Ptr VkFramebufferCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkFramebuffer -> IO VkResult))
  , fp_vkDestroyFramebuffer :: !(FunPtr (VkDevice -> VkFramebuffer -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkCreateRenderPass :: !(FunPtr (VkDevice -> Ptr VkRenderPassCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkRenderPass -> IO VkResult))
  , fp_vkDestroyRenderPass :: !(FunPtr (VkDevice -> VkRenderPass -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkGetRenderAreaGranularity :: !(FunPtr (VkDevice -> VkRenderPass -> Ptr VkExtent2D -> IO ()))
  , fp_vkCreateCommandPool :: !(FunPtr (VkDevice -> Ptr VkCommandPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkCommandPool -> IO VkResult))
  , fp_vkDestroyCommandPool :: !(FunPtr (VkDevice -> VkCommandPool -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkResetCommandPool :: !(FunPtr (VkDevice -> VkCommandPool -> VkCommandPoolResetFlags -> IO VkResult))
  , fp_vkAllocateCommandBuffers :: !(FunPtr (VkDevice -> Ptr VkCommandBufferAllocateInfo -> Ptr VkCommandBuffer -> IO VkResult))
  , fp_vkFreeCommandBuffers :: !(FunPtr (VkDevice -> VkCommandPool -> Word32 -> Ptr VkCommandBuffer -> IO ()))
  , fp_vkBeginCommandBuffer :: !(FunPtr (VkCommandBuffer -> Ptr VkCommandBufferBeginInfo -> IO VkResult))
  , fp_vkEndCommandBuffer :: !(FunPtr (VkCommandBuffer -> IO VkResult))
  , fp_vkResetCommandBuffer :: !(FunPtr (VkCommandBuffer -> VkCommandBufferResetFlags -> IO VkResult))
  , fp_vkCmdBindPipeline :: !(FunPtr (VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> IO ()))
  , fp_vkCmdSetViewport :: !(FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkViewport -> IO ()))
  , fp_vkCmdSetScissor :: !(FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkRect2D -> IO ()))
  , fp_vkCmdSetLineWidth :: !(FunPtr (VkCommandBuffer -> Float -> IO ()))
  , fp_vkCmdSetDepthBias :: !(FunPtr (VkCommandBuffer -> Float -> Float -> Float -> IO ()))
  , fp_vkCmdSetBlendConstants :: !(FunPtr (VkCommandBuffer -> Float -> Float -> Float -> Float -> IO ()))
  , fp_vkCmdSetDepthBounds :: !(FunPtr (VkCommandBuffer -> Float -> Float -> IO ()))
  , fp_vkCmdSetStencilCompareMask :: !(FunPtr (VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()))
  , fp_vkCmdSetStencilWriteMask :: !(FunPtr (VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()))
  , fp_vkCmdSetStencilReference :: !(FunPtr (VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()))
  , fp_vkCmdBindDescriptorSets :: !(FunPtr (VkCommandBuffer -> VkPipelineBindPoint -> VkPipelineLayout -> Word32 -> Word32 -> Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ()))
  , fp_vkCmdBindIndexBuffer :: !(FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkIndexType -> IO ()))
  , fp_vkCmdBindVertexBuffers :: !(FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkBuffer -> Ptr VkDeviceSize -> IO ()))
  , fp_vkCmdDraw :: !(FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()))
  , fp_vkCmdDrawIndexed :: !(FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()))
  , fp_vkCmdDrawIndirect :: !(FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()))
  , fp_vkCmdDrawIndexedIndirect :: !(FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()))
  , fp_vkCmdDispatch :: !(FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> IO ()))
  , fp_vkCmdDispatchIndirect :: !(FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> IO ()))
  , fp_vkCmdCopyBuffer :: !(FunPtr (VkCommandBuffer -> VkBuffer -> VkBuffer -> Word32 -> Ptr VkBufferCopy -> IO ()))
  , fp_vkCmdCopyImage :: !(FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageCopy -> IO ()))
  , fp_vkCmdBlitImage :: !(FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageBlit -> VkFilter -> IO ()))
  , fp_vkCmdCopyBufferToImage :: !(FunPtr (VkCommandBuffer -> VkBuffer -> VkImage -> VkImageLayout -> Word32 -> Ptr VkBufferImageCopy -> IO ()))
  , fp_vkCmdCopyImageToBuffer :: !(FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> VkBuffer -> Word32 -> Ptr VkBufferImageCopy -> IO ()))
  , fp_vkCmdUpdateBuffer :: !(FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> Ptr Word32 -> IO ()))
  , fp_vkCmdFillBuffer :: !(FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> Word32 -> IO ()))
  , fp_vkCmdClearColorImage :: !(FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> Ptr VkClearColorValue -> Word32 -> Ptr VkImageSubresourceRange -> IO ()))
  , fp_vkCmdClearDepthStencilImage :: !(FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> Ptr VkClearDepthStencilValue -> Word32 -> Ptr VkImageSubresourceRange -> IO ()))
  , fp_vkCmdClearAttachments :: !(FunPtr (VkCommandBuffer -> Word32 -> Ptr VkClearAttachment -> Word32 -> Ptr VkClearRect -> IO ()))
  , fp_vkCmdResolveImage :: !(FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageResolve -> IO ()))
  , fp_vkCmdSetEvent :: !(FunPtr (VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ()))
  , fp_vkCmdResetEvent :: !(FunPtr (VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ()))
  , fp_vkCmdWaitEvents :: !(FunPtr (VkCommandBuffer -> Word32 -> Ptr VkEvent -> VkPipelineStageFlags -> VkPipelineStageFlags -> Word32 -> Ptr VkMemoryBarrier -> Word32 -> Ptr VkBufferMemoryBarrier -> Word32 -> Ptr VkImageMemoryBarrier -> IO ()))
  , fp_vkCmdPipelineBarrier :: !(FunPtr (VkCommandBuffer -> VkPipelineStageFlags -> VkPipelineStageFlags -> VkDependencyFlags -> Word32 -> Ptr VkMemoryBarrier -> Word32 -> Ptr VkBufferMemoryBarrier -> Word32 -> Ptr VkImageMemoryBarrier -> IO ()))
  , fp_vkCmdBeginQuery :: !(FunPtr (VkCommandBuffer -> VkQueryPool -> Word32 -> VkQueryControlFlags -> IO ()))
  , fp_vkCmdEndQuery :: !(FunPtr (VkCommandBuffer -> VkQueryPool -> Word32 -> IO ()))
  , fp_vkCmdResetQueryPool :: !(FunPtr (VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> IO ()))
  , fp_vkCmdWriteTimestamp :: !(FunPtr (VkCommandBuffer -> VkPipelineStageFlagBits -> VkQueryPool -> Word32 -> IO ()))
  , fp_vkCmdCopyQueryPoolResults :: !(FunPtr (VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> VkQueryResultFlags -> IO ()))
  , fp_vkCmdPushConstants :: !(FunPtr (VkCommandBuffer -> VkPipelineLayout -> VkShaderStageFlags -> Word32 -> Word32 -> Ptr () -> IO ()))
  , fp_vkCmdBeginRenderPass :: !(FunPtr (VkCommandBuffer -> Ptr VkRenderPassBeginInfo -> VkSubpassContents -> IO ()))
  , fp_vkCmdNextSubpass :: !(FunPtr (VkCommandBuffer -> VkSubpassContents -> IO ()))
  , fp_vkCmdEndRenderPass :: !(FunPtr (VkCommandBuffer -> IO ()))
  , fp_vkCmdExecuteCommands :: !(FunPtr (VkCommandBuffer -> Word32 -> Ptr VkCommandBuffer -> IO ()))
  , fp_vkCreateSharedSwapchainsKHR :: !(FunPtr (VkDevice -> Word32 -> Ptr VkSwapchainCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult))
  , fp_vkCreateSwapchainKHR :: !(FunPtr (VkDevice -> Ptr VkSwapchainCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult))
  , fp_vkDestroySwapchainKHR :: !(FunPtr (VkDevice -> VkSwapchainKHR -> Ptr VkAllocationCallbacks -> IO ()))
  , fp_vkGetSwapchainImagesKHR :: !(FunPtr (VkDevice -> VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO VkResult))
  , fp_vkAcquireNextImageKHR :: !(FunPtr (VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO VkResult))
  , fp_vkQueuePresentKHR :: !(FunPtr (VkQueue -> Ptr VkPresentInfoKHR -> IO VkResult))
  , fp_vkDebugMarkerSetObjectNameEXT :: !(FunPtr (VkDevice -> Ptr VkDebugMarkerObjectNameInfoEXT -> IO VkResult))
  , fp_vkDebugMarkerSetObjectTagEXT :: !(FunPtr (VkDevice -> Ptr VkDebugMarkerObjectTagInfoEXT -> IO VkResult))
  , fp_vkCmdDebugMarkerBeginEXT :: !(FunPtr (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()))
  , fp_vkCmdDebugMarkerEndEXT :: !(FunPtr (VkCommandBuffer -> IO ()))
  , fp_vkCmdDebugMarkerInsertEXT :: !(FunPtr (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()))
  }

getVulkan :: MonadIO m => VulkanSetup -> VkDevice -> m Vulkan
getVulkan vks device = liftIO $ do
  let getDeviceProcAddr proc =
        return . castFunPtr =<< withCAString proc (vkGetDeviceProcAddr vks device)
  return Vulkan
    <*> getDeviceProcAddr "vkDestroyDevice"
    <*> getDeviceProcAddr "vkGetDeviceQueue"
    <*> getDeviceProcAddr "vkQueueSubmit"
    <*> getDeviceProcAddr "vkQueueWaitIdle"
    <*> getDeviceProcAddr "vkDeviceWaitIdle"
    <*> getDeviceProcAddr "vkAllocateMemory"
    <*> getDeviceProcAddr "vkFreeMemory"
    <*> getDeviceProcAddr "vkMapMemory"
    <*> getDeviceProcAddr "vkUnmapMemory"
    <*> getDeviceProcAddr "vkFlushMappedMemoryRanges"
    <*> getDeviceProcAddr "vkInvalidateMappedMemoryRanges"
    <*> getDeviceProcAddr "vkGetDeviceMemoryCommitment"
    <*> getDeviceProcAddr "vkGetBufferMemoryRequirements"
    <*> getDeviceProcAddr "vkBindBufferMemory"
    <*> getDeviceProcAddr "vkGetImageMemoryRequirements"
    <*> getDeviceProcAddr "vkBindImageMemory"
    <*> getDeviceProcAddr "vkGetImageSparseMemoryRequirements"
    <*> getDeviceProcAddr "vkQueueBindSparse"
    <*> getDeviceProcAddr "vkCreateFence"
    <*> getDeviceProcAddr "vkDestroyFence"
    <*> getDeviceProcAddr "vkResetFences"
    <*> getDeviceProcAddr "vkGetFenceStatus"
    <*> getDeviceProcAddr "vkWaitForFences"
    <*> getDeviceProcAddr "vkCreateSemaphore"
    <*> getDeviceProcAddr "vkDestroySemaphore"
    <*> getDeviceProcAddr "vkCreateEvent"
    <*> getDeviceProcAddr "vkDestroyEvent"
    <*> getDeviceProcAddr "vkGetEventStatus"
    <*> getDeviceProcAddr "vkSetEvent"
    <*> getDeviceProcAddr "vkResetEvent"
    <*> getDeviceProcAddr "vkCreateQueryPool"
    <*> getDeviceProcAddr "vkDestroyQueryPool"
    <*> getDeviceProcAddr "vkGetQueryPoolResults"
    <*> getDeviceProcAddr "vkCreateBuffer"
    <*> getDeviceProcAddr "vkDestroyBuffer"
    <*> getDeviceProcAddr "vkCreateBufferView"
    <*> getDeviceProcAddr "vkDestroyBufferView"
    <*> getDeviceProcAddr "vkCreateImage"
    <*> getDeviceProcAddr "vkDestroyImage"
    <*> getDeviceProcAddr "vkGetImageSubresourceLayout"
    <*> getDeviceProcAddr "vkCreateImageView"
    <*> getDeviceProcAddr "vkDestroyImageView"
    <*> getDeviceProcAddr "vkCreateShaderModule"
    <*> getDeviceProcAddr "vkDestroyShaderModule"
    <*> getDeviceProcAddr "vkCreatePipelineCache"
    <*> getDeviceProcAddr "vkDestroyPipelineCache"
    <*> getDeviceProcAddr "vkGetPipelineCacheData"
    <*> getDeviceProcAddr "vkMergePipelineCaches"
    <*> getDeviceProcAddr "vkCreateGraphicsPipelines"
    <*> getDeviceProcAddr "vkCreateComputePipelines"
    <*> getDeviceProcAddr "vkDestroyPipeline"
    <*> getDeviceProcAddr "vkCreatePipelineLayout"
    <*> getDeviceProcAddr "vkDestroyPipelineLayout"
    <*> getDeviceProcAddr "vkCreateSampler"
    <*> getDeviceProcAddr "vkDestroySampler"
    <*> getDeviceProcAddr "vkCreateDescriptorSetLayout"
    <*> getDeviceProcAddr "vkDestroyDescriptorSetLayout"
    <*> getDeviceProcAddr "vkCreateDescriptorPool"
    <*> getDeviceProcAddr "vkDestroyDescriptorPool"
    <*> getDeviceProcAddr "vkResetDescriptorPool"
    <*> getDeviceProcAddr "vkAllocateDescriptorSets"
    <*> getDeviceProcAddr "vkFreeDescriptorSets"
    <*> getDeviceProcAddr "vkUpdateDescriptorSets"
    <*> getDeviceProcAddr "vkCreateFramebuffer"
    <*> getDeviceProcAddr "vkDestroyFramebuffer"
    <*> getDeviceProcAddr "vkCreateRenderPass"
    <*> getDeviceProcAddr "vkDestroyRenderPass"
    <*> getDeviceProcAddr "vkGetRenderAreaGranularity"
    <*> getDeviceProcAddr "vkCreateCommandPool"
    <*> getDeviceProcAddr "vkDestroyCommandPool"
    <*> getDeviceProcAddr "vkResetCommandPool"
    <*> getDeviceProcAddr "vkAllocateCommandBuffers"
    <*> getDeviceProcAddr "vkFreeCommandBuffers"
    <*> getDeviceProcAddr "vkBeginCommandBuffer"
    <*> getDeviceProcAddr "vkEndCommandBuffer"
    <*> getDeviceProcAddr "vkResetCommandBuffer"
    <*> getDeviceProcAddr "vkCmdBindPipeline"
    <*> getDeviceProcAddr "vkCmdSetViewport"
    <*> getDeviceProcAddr "vkCmdSetScissor"
    <*> getDeviceProcAddr "vkCmdSetLineWidth"
    <*> getDeviceProcAddr "vkCmdSetDepthBias"
    <*> getDeviceProcAddr "vkCmdSetBlendConstants"
    <*> getDeviceProcAddr "vkCmdSetDepthBounds"
    <*> getDeviceProcAddr "vkCmdSetStencilCompareMask"
    <*> getDeviceProcAddr "vkCmdSetStencilWriteMask"
    <*> getDeviceProcAddr "vkCmdSetStencilReference"
    <*> getDeviceProcAddr "vkCmdBindDescriptorSets"
    <*> getDeviceProcAddr "vkCmdBindIndexBuffer"
    <*> getDeviceProcAddr "vkCmdBindVertexBuffers"
    <*> getDeviceProcAddr "vkCmdDraw"
    <*> getDeviceProcAddr "vkCmdDrawIndexed"
    <*> getDeviceProcAddr "vkCmdDrawIndirect"
    <*> getDeviceProcAddr "vkCmdDrawIndexedIndirect"
    <*> getDeviceProcAddr "vkCmdDispatch"
    <*> getDeviceProcAddr "vkCmdDispatchIndirect"
    <*> getDeviceProcAddr "vkCmdCopyBuffer"
    <*> getDeviceProcAddr "vkCmdCopyImage"
    <*> getDeviceProcAddr "vkCmdBlitImage"
    <*> getDeviceProcAddr "vkCmdCopyBufferToImage"
    <*> getDeviceProcAddr "vkCmdCopyImageToBuffer"
    <*> getDeviceProcAddr "vkCmdUpdateBuffer"
    <*> getDeviceProcAddr "vkCmdFillBuffer"
    <*> getDeviceProcAddr "vkCmdClearColorImage"
    <*> getDeviceProcAddr "vkCmdClearDepthStencilImage"
    <*> getDeviceProcAddr "vkCmdClearAttachments"
    <*> getDeviceProcAddr "vkCmdResolveImage"
    <*> getDeviceProcAddr "vkCmdSetEvent"
    <*> getDeviceProcAddr "vkCmdResetEvent"
    <*> getDeviceProcAddr "vkCmdWaitEvents"
    <*> getDeviceProcAddr "vkCmdPipelineBarrier"
    <*> getDeviceProcAddr "vkCmdBeginQuery"
    <*> getDeviceProcAddr "vkCmdEndQuery"
    <*> getDeviceProcAddr "vkCmdResetQueryPool"
    <*> getDeviceProcAddr "vkCmdWriteTimestamp"
    <*> getDeviceProcAddr "vkCmdCopyQueryPoolResults"
    <*> getDeviceProcAddr "vkCmdPushConstants"
    <*> getDeviceProcAddr "vkCmdBeginRenderPass"
    <*> getDeviceProcAddr "vkCmdNextSubpass"
    <*> getDeviceProcAddr "vkCmdEndRenderPass"
    <*> getDeviceProcAddr "vkCmdExecuteCommands"
    <*> getDeviceProcAddr "vkCreateSharedSwapchainsKHR"
    <*> getDeviceProcAddr "vkCreateSwapchainKHR"
    <*> getDeviceProcAddr "vkDestroySwapchainKHR"
    <*> getDeviceProcAddr "vkGetSwapchainImagesKHR"
    <*> getDeviceProcAddr "vkAcquireNextImageKHR"
    <*> getDeviceProcAddr "vkQueuePresentKHR"
    <*> getDeviceProcAddr "vkDebugMarkerSetObjectNameEXT"
    <*> getDeviceProcAddr "vkDebugMarkerSetObjectTagEXT"
    <*> getDeviceProcAddr "vkCmdDebugMarkerBeginEXT"
    <*> getDeviceProcAddr "vkCmdDebugMarkerEndEXT"
    <*> getDeviceProcAddr "vkCmdDebugMarkerInsertEXT"

type VkSampleMask = Word32
type VkBool32 = Word32
type VkFlags = Word32
type VkDeviceSize = Word64
-- | Requires: 
type VkFramebufferCreateFlags = VkFlags
-- | Requires: 
type VkQueryPoolCreateFlags = VkFlags
-- | Requires: 
type VkRenderPassCreateFlags = VkFlags
-- | Requires: 
type VkSamplerCreateFlags = VkFlags
-- | Requires: 
type VkPipelineLayoutCreateFlags = VkFlags
-- | Requires: 
type VkPipelineCacheCreateFlags = VkFlags
-- | Requires: 
type VkPipelineDepthStencilStateCreateFlags = VkFlags
-- | Requires: 
type VkPipelineDynamicStateCreateFlags = VkFlags
-- | Requires: 
type VkPipelineColorBlendStateCreateFlags = VkFlags
-- | Requires: 
type VkPipelineMultisampleStateCreateFlags = VkFlags
-- | Requires: 
type VkPipelineRasterizationStateCreateFlags = VkFlags
-- | Requires: 
type VkPipelineViewportStateCreateFlags = VkFlags
-- | Requires: 
type VkPipelineTessellationStateCreateFlags = VkFlags
-- | Requires: 
type VkPipelineInputAssemblyStateCreateFlags = VkFlags
-- | Requires: 
type VkPipelineVertexInputStateCreateFlags = VkFlags
-- | Requires: 
type VkPipelineShaderStageCreateFlags = VkFlags
-- | Requires: 
type VkDescriptorSetLayoutCreateFlags = VkFlags
-- | Requires: 
type VkBufferViewCreateFlags = VkFlags
-- | Requires: 
type VkInstanceCreateFlags = VkFlags
-- | Requires: 
type VkDeviceCreateFlags = VkFlags
-- | Requires: 
type VkDeviceQueueCreateFlags = VkFlags
-- | Requires: VkQueueFlagBits
type VkQueueFlags = VkFlags
-- | Requires: VkMemoryPropertyFlagBits
type VkMemoryPropertyFlags = VkFlags
-- | Requires: VkMemoryHeapFlagBits
type VkMemoryHeapFlags = VkFlags
-- | Requires: VkAccessFlagBits
type VkAccessFlags = VkFlags
-- | Requires: VkBufferUsageFlagBits
type VkBufferUsageFlags = VkFlags
-- | Requires: VkBufferCreateFlagBits
type VkBufferCreateFlags = VkFlags
-- | Requires: VkShaderStageFlagBits
type VkShaderStageFlags = VkFlags
-- | Requires: VkImageUsageFlagBits
type VkImageUsageFlags = VkFlags
-- | Requires: VkImageCreateFlagBits
type VkImageCreateFlags = VkFlags
-- | Requires: 
type VkImageViewCreateFlags = VkFlags
-- | Requires: VkPipelineCreateFlagBits
type VkPipelineCreateFlags = VkFlags
-- | Requires: VkColorComponentFlagBits
type VkColorComponentFlags = VkFlags
-- | Requires: VkFenceCreateFlagBits
type VkFenceCreateFlags = VkFlags
-- | Requires: 
type VkSemaphoreCreateFlags = VkFlags
-- | Requires: VkFormatFeatureFlagBits
type VkFormatFeatureFlags = VkFlags
-- | Requires: VkQueryControlFlagBits
type VkQueryControlFlags = VkFlags
-- | Requires: VkQueryResultFlagBits
type VkQueryResultFlags = VkFlags
-- | Requires: 
type VkShaderModuleCreateFlags = VkFlags
-- | Requires: 
type VkEventCreateFlags = VkFlags
-- | Requires: VkCommandPoolCreateFlagBits
type VkCommandPoolCreateFlags = VkFlags
-- | Requires: VkCommandPoolResetFlagBits
type VkCommandPoolResetFlags = VkFlags
-- | Requires: VkCommandBufferResetFlagBits
type VkCommandBufferResetFlags = VkFlags
-- | Requires: VkCommandBufferUsageFlagBits
type VkCommandBufferUsageFlags = VkFlags
-- | Requires: VkQueryPipelineStatisticFlagBits
type VkQueryPipelineStatisticFlags = VkFlags
-- | Requires: 
type VkMemoryMapFlags = VkFlags
-- | Requires: VkImageAspectFlagBits
type VkImageAspectFlags = VkFlags
-- | Requires: VkSparseMemoryBindFlagBits
type VkSparseMemoryBindFlags = VkFlags
-- | Requires: VkSparseImageFormatFlagBits
type VkSparseImageFormatFlags = VkFlags
-- | Requires: 
type VkSubpassDescriptionFlags = VkFlags
-- | Requires: VkPipelineStageFlagBits
type VkPipelineStageFlags = VkFlags
-- | Requires: VkSampleCountFlagBits
type VkSampleCountFlags = VkFlags
-- | Requires: VkAttachmentDescriptionFlagBits
type VkAttachmentDescriptionFlags = VkFlags
-- | Requires: VkStencilFaceFlagBits
type VkStencilFaceFlags = VkFlags
-- | Requires: VkCullModeFlagBits
type VkCullModeFlags = VkFlags
-- | Requires: VkDescriptorPoolCreateFlagBits
type VkDescriptorPoolCreateFlags = VkFlags
-- | Requires: 
type VkDescriptorPoolResetFlags = VkFlags
-- | Requires: VkDependencyFlagBits
type VkDependencyFlags = VkFlags
-- | Requires: VkCompositeAlphaFlagBitsKHR
type VkCompositeAlphaFlagsKHR = VkFlags
-- | Requires: VkDisplayPlaneAlphaFlagBitsKHR
type VkDisplayPlaneAlphaFlagsKHR = VkFlags
-- | Requires: VkSurfaceTransformFlagBitsKHR
type VkSurfaceTransformFlagsKHR = VkFlags
-- | Requires: 
type VkSwapchainCreateFlagsKHR = VkFlags
-- | Requires: 
type VkDisplayModeCreateFlagsKHR = VkFlags
-- | Requires: 
type VkDisplaySurfaceCreateFlagsKHR = VkFlags
-- | Requires: 
type VkAndroidSurfaceCreateFlagsKHR = VkFlags
-- | Requires: 
type VkMirSurfaceCreateFlagsKHR = VkFlags
-- | Requires: 
type VkWaylandSurfaceCreateFlagsKHR = VkFlags
-- | Requires: 
type VkWin32SurfaceCreateFlagsKHR = VkFlags
-- | Requires: 
type VkXlibSurfaceCreateFlagsKHR = VkFlags
-- | Requires: 
type VkXcbSurfaceCreateFlagsKHR = VkFlags
-- | Requires: VkDebugReportFlagBitsEXT
type VkDebugReportFlagsEXT = VkFlags
-- | Parent: Nothing
type VkInstance = Ptr ()
-- | Parent: Just "VkInstance"
type VkPhysicalDevice = Ptr ()
-- | Parent: Just "VkPhysicalDevice"
type VkDevice = Ptr ()
-- | Parent: Just "VkDevice"
type VkQueue = Ptr ()
-- | Parent: Just "VkCommandPool"
type VkCommandBuffer = Ptr ()
-- | Parent: Just "VkDevice"
type VkDeviceMemory = Int64
-- | Parent: Just "VkDevice"
type VkCommandPool = Int64
-- | Parent: Just "VkDevice"
type VkBuffer = Int64
-- | Parent: Just "VkDevice"
type VkBufferView = Int64
-- | Parent: Just "VkDevice"
type VkImage = Int64
-- | Parent: Just "VkDevice"
type VkImageView = Int64
-- | Parent: Just "VkDevice"
type VkShaderModule = Int64
-- | Parent: Just "VkDevice"
type VkPipeline = Int64
-- | Parent: Just "VkDevice"
type VkPipelineLayout = Int64
-- | Parent: Just "VkDevice"
type VkSampler = Int64
-- | Parent: Just "VkDescriptorPool"
type VkDescriptorSet = Int64
-- | Parent: Just "VkDevice"
type VkDescriptorSetLayout = Int64
-- | Parent: Just "VkDevice"
type VkDescriptorPool = Int64
-- | Parent: Just "VkDevice"
type VkFence = Int64
-- | Parent: Just "VkDevice"
type VkSemaphore = Int64
-- | Parent: Just "VkDevice"
type VkEvent = Int64
-- | Parent: Just "VkDevice"
type VkQueryPool = Int64
-- | Parent: Just "VkDevice"
type VkFramebuffer = Int64
-- | Parent: Just "VkDevice"
type VkRenderPass = Int64
-- | Parent: Just "VkDevice"
type VkPipelineCache = Int64
-- | Parent: Nothing
type VkDisplayKHR = Int64
-- | Parent: Just "VkPhysicalDevice,VkDisplayKHR"
type VkDisplayModeKHR = Int64
-- | Parent: Just "VkInstance"
type VkSurfaceKHR = Int64
-- | Parent: Just "VkSurfaceKHR"
type VkSwapchainKHR = Int64
-- | Parent: Just "VkInstance"
type VkDebugReportCallbackEXT = Int64
type PFN_vkInternalAllocationNotification = FunPtr (Ptr () -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())
foreign import ccall unsafe "wrapper" ffi_PFN_vkInternalAllocationNotification :: (Ptr () -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ()) -> IO (FunPtr (Ptr () -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ()))
type PFN_vkInternalFreeNotification = FunPtr (Ptr () -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ())
foreign import ccall unsafe "wrapper" ffi_PFN_vkInternalFreeNotification :: (Ptr () -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ()) -> IO (FunPtr (Ptr () -> CSize -> VkInternalAllocationType -> VkSystemAllocationScope -> IO ()))
type PFN_vkReallocationFunction = FunPtr (Ptr () -> Ptr () -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr ()))
foreign import ccall unsafe "wrapper" ffi_PFN_vkReallocationFunction :: (Ptr () -> Ptr () -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr ())) -> IO (FunPtr (Ptr () -> Ptr () -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr ())))
type PFN_vkAllocationFunction = FunPtr (Ptr () -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr ()))
foreign import ccall unsafe "wrapper" ffi_PFN_vkAllocationFunction :: (Ptr () -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr ())) -> IO (FunPtr (Ptr () -> CSize -> CSize -> VkSystemAllocationScope -> IO (Ptr ())))
type PFN_vkFreeFunction = FunPtr (Ptr () -> Ptr () -> IO ())
foreign import ccall unsafe "wrapper" ffi_PFN_vkFreeFunction :: (Ptr () -> Ptr () -> IO ()) -> IO (FunPtr (Ptr () -> Ptr () -> IO ()))
type PFN_vkVoidFunction = FunPtr (IO ())
foreign import ccall unsafe "wrapper" ffi_PFN_vkVoidFunction :: (IO ()) -> IO (FunPtr (IO ()))
type PFN_vkDebugReportCallbackEXT = FunPtr (VkDebugReportFlagsEXT -> VkDebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> CChar -> CChar -> Ptr () -> IO VkBool32)
foreign import ccall unsafe "wrapper" ffi_PFN_vkDebugReportCallbackEXT :: (VkDebugReportFlagsEXT -> VkDebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> CChar -> CChar -> Ptr () -> IO VkBool32) -> IO (FunPtr (VkDebugReportFlagsEXT -> VkDebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> CChar -> CChar -> Ptr () -> IO VkBool32))

-- | Man not found. VkOffset2D
-- 
data VkOffset2D = VkOffset2D
  { vo2d_x :: !Int32
  -- ^ `x` 
  , vo2d_y :: !Int32
  -- ^ `y` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkOffset2D where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    x <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    y <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    return $ VkOffset2D x y
  poke ptr (VkOffset2D x y) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) x
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) y


-- | Man not found. VkOffset3D
-- 
data VkOffset3D = VkOffset3D
  { vo3d_x :: !Int32
  -- ^ `x` 
  , vo3d_y :: !Int32
  -- ^ `y` 
  , vo3d_z :: !Int32
  -- ^ `z` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkOffset3D where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    x <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    y <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    z <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkOffset3D x y z
  poke ptr (VkOffset3D x y z) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) x
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) y
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) z


-- | Man not found. VkExtent2D
-- 
data VkExtent2D = VkExtent2D
  { ve2d_width :: !Word32
  -- ^ `width` 
  , ve2d_height :: !Word32
  -- ^ `height` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkExtent2D where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    width <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    height <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    return $ VkExtent2D width height
  poke ptr (VkExtent2D width height) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) width
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) height


-- | Man not found. VkExtent3D
-- 
data VkExtent3D = VkExtent3D
  { ve3d_width :: !Word32
  -- ^ `width` 
  , ve3d_height :: !Word32
  -- ^ `height` 
  , ve3d_depth :: !Word32
  -- ^ `depth` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkExtent3D where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    width <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    height <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    depth <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkExtent3D width height depth
  poke ptr (VkExtent3D width height depth) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) width
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) height
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) depth


-- | Man not found. VkViewport
-- 
-- == Validaty
-- * pname:width must: be greater than `0.0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxViewportDimensions[0]
-- * pname:height must: be greater than `0.0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxViewportDimensions[1]
-- * pname:x and pname:y must: each be between pname:viewportBoundsRange[0] and pname:viewportBoundsRange[1], inclusive
-- * pname:x + pname:width must: be less than or equal to pname:viewportBoundsRange[1]
-- * pname:y + pname:height must: be less than or equal to pname:viewportBoundsRange[1]
-- * pname:minDepth must: be between `0.0` and `1.0`, inclusive
-- * pname:maxDepth must: be between `0.0` and `1.0`, inclusive
data VkViewport = VkViewport
  { vv_x :: !Float
  -- ^ `x` 
  , vv_y :: !Float
  -- ^ `y` 
  , vv_width :: !Float
  -- ^ `width` 
  , vv_height :: !Float
  -- ^ `height` 
  , vv_minDepth :: !Float
  -- ^ `minDepth` 
  , vv_maxDepth :: !Float
  -- ^ `maxDepth` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkViewport where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 24 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    x <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    y <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    width <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    height <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    minDepth <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    maxDepth <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0))
    return $ VkViewport x y width height minDepth maxDepth
  poke ptr (VkViewport x y width height minDepth maxDepth) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) x
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) y
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) width
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) height
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) minDepth
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0)) maxDepth


-- | Man not found. VkRect2D
-- 
data VkRect2D = VkRect2D
  { vr2d_offset :: !VkOffset2D
  -- ^ `offset` 
  , vr2d_extent :: !VkExtent2D
  -- ^ `extent` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkRect2D where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    offset <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    extent <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkRect2D offset extent
  poke ptr (VkRect2D offset extent) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) offset
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) extent


-- | Man not found. VkRect3D
-- 
data VkRect3D = VkRect3D
  { vr3d_offset :: !VkOffset3D
  -- ^ `offset` 
  , vr3d_extent :: !VkExtent3D
  -- ^ `extent` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkRect3D where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 24 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    offset <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    extent <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    return $ VkRect3D offset extent
  poke ptr (VkRect3D offset extent) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) offset
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) extent


-- | Man not found. VkClearRect
-- 
data VkClearRect = VkClearRect
  { vcr_rect :: !VkRect2D
  -- ^ `rect` 
  , vcr_baseArrayLayer :: !Word32
  -- ^ `baseArrayLayer` 
  , vcr_layerCount :: !Word32
  -- ^ `layerCount` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkClearRect where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 24 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    rect <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    baseArrayLayer <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    layerCount <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0))
    return $ VkClearRect rect baseArrayLayer layerCount
  poke ptr (VkClearRect rect baseArrayLayer layerCount) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) rect
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) baseArrayLayer
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0)) layerCount


-- | Man not found. VkComponentMapping
-- 
data VkComponentMapping = VkComponentMapping
  { vcm_r :: !VkComponentSwizzle
  -- ^ `r` 
  , vcm_g :: !VkComponentSwizzle
  -- ^ `g` 
  , vcm_b :: !VkComponentSwizzle
  -- ^ `b` 
  , vcm_a :: !VkComponentSwizzle
  -- ^ `a` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkComponentMapping where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 0 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    r <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    g <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    b <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    a <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3))
    return $ VkComponentMapping r g b a
  poke ptr (VkComponentMapping r g b a) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) r
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) g
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) b
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3)) a


-- | Man not found. VkPhysicalDeviceProperties
-- 
data VkPhysicalDeviceProperties = VkPhysicalDeviceProperties
  { vpdp_apiVersion :: !Word32
  -- ^ `apiVersion` 
  , vpdp_driverVersion :: !Word32
  -- ^ `driverVersion` 
  , vpdp_vendorID :: !Word32
  -- ^ `vendorID` 
  , vpdp_deviceID :: !Word32
  -- ^ `deviceID` 
  , vpdp_deviceType :: !VkPhysicalDeviceType
  -- ^ `deviceType` 
  , vpdp_deviceName :: !(V 256 CChar)
  -- ^ `deviceName`  Max: VK_MAX_PHYSICAL_DEVICE_NAME_SIZE.
  , vpdp_pipelineCacheUUID :: !(V 16 Word8)
  -- ^ `pipelineCacheUUID`  Max: VK_UUID_SIZE.
  , vpdp_limits :: !VkPhysicalDeviceLimits
  -- ^ `limits` 
  , vpdp_sparseProperties :: !VkPhysicalDeviceSparseProperties
  -- ^ `sparseProperties` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkPhysicalDeviceProperties where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 792 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    apiVersion <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    driverVersion <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    vendorID <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    deviceID <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    deviceType <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    deviceName <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 1))
    pipelineCacheUUID <- peek (plusPtr ptr (272 + sizeOf (undefined :: CSize) * 1))
    limits <- peek (plusPtr ptr (288 + sizeOf (undefined :: CSize) * 1))
    sparseProperties <- peek (plusPtr ptr (772 + sizeOf (undefined :: CSize) * 2))
    return $ VkPhysicalDeviceProperties apiVersion driverVersion vendorID deviceID deviceType deviceName pipelineCacheUUID limits sparseProperties
  poke ptr (VkPhysicalDeviceProperties apiVersion driverVersion vendorID deviceID deviceType deviceName pipelineCacheUUID limits sparseProperties) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) apiVersion
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) driverVersion
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) vendorID
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) deviceID
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) deviceType
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 1)) deviceName
    poke (plusPtr ptr (272 + sizeOf (undefined :: CSize) * 1)) pipelineCacheUUID
    poke (plusPtr ptr (288 + sizeOf (undefined :: CSize) * 1)) limits
    poke (plusPtr ptr (772 + sizeOf (undefined :: CSize) * 2)) sparseProperties


-- | Man not found. VkExtensionProperties
-- 
data VkExtensionProperties = VkExtensionProperties
  { vep_extensionName :: !(V 256 CChar)
  -- ^ `extensionName`  Max: VK_MAX_EXTENSION_NAME_SIZE.
  , vep_specVersion :: !Word32
  -- ^ `specVersion` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkExtensionProperties where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 260 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    extensionName <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    specVersion <- peek (plusPtr ptr (256 + sizeOf (undefined :: CSize) * 0))
    return $ VkExtensionProperties extensionName specVersion
  poke ptr (VkExtensionProperties extensionName specVersion) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) extensionName
    poke (plusPtr ptr (256 + sizeOf (undefined :: CSize) * 0)) specVersion


-- | Man not found. VkLayerProperties
-- 
data VkLayerProperties = VkLayerProperties
  { vlp_layerName :: !(V 256 CChar)
  -- ^ `layerName`  Max: VK_MAX_EXTENSION_NAME_SIZE.
  , vlp_specVersion :: !Word32
  -- ^ `specVersion` 
  , vlp_implementationVersion :: !Word32
  -- ^ `implementationVersion` 
  , vlp_description :: !(V 256 CChar)
  -- ^ `description`  Max: VK_MAX_DESCRIPTION_SIZE.
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkLayerProperties where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 520 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    layerName <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    specVersion <- peek (plusPtr ptr (256 + sizeOf (undefined :: CSize) * 0))
    implementationVersion <- peek (plusPtr ptr (260 + sizeOf (undefined :: CSize) * 0))
    description <- peek (plusPtr ptr (264 + sizeOf (undefined :: CSize) * 0))
    return $ VkLayerProperties layerName specVersion implementationVersion description
  poke ptr (VkLayerProperties layerName specVersion implementationVersion description) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) layerName
    poke (plusPtr ptr (256 + sizeOf (undefined :: CSize) * 0)) specVersion
    poke (plusPtr ptr (260 + sizeOf (undefined :: CSize) * 0)) implementationVersion
    poke (plusPtr ptr (264 + sizeOf (undefined :: CSize) * 0)) description


-- | Man not found. VkApplicationInfo
-- 
-- == Validaty
-- * pname:apiVersion must: be zero, or otherwise it must: be a version that the implementation supports, or supports an effective substitute for
data VkApplicationInfo = VkApplicationInfo
  { vai_sType :: !VkStructureType
  -- ^ `sType` 
  , vai_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vai_pApplicationName :: !(Ptr CChar)
  -- ^ `pApplicationName`  Can be `nullPtr`. Const. Length: null-terminated.
  , vai_applicationVersion :: !Word32
  -- ^ `applicationVersion` 
  , vai_pEngineName :: !(Ptr CChar)
  -- ^ `pEngineName`  Can be `nullPtr`. Const. Length: null-terminated.
  , vai_engineVersion :: !Word32
  -- ^ `engineVersion` 
  , vai_apiVersion :: !Word32
  -- ^ `apiVersion` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkApplicationInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    pApplicationName <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    applicationVersion <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3))
    pEngineName <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    engineVersion <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4))
    apiVersion <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4))
    return $ VkApplicationInfo sType pNext pApplicationName applicationVersion pEngineName engineVersion apiVersion
  poke ptr (VkApplicationInfo sType pNext pApplicationName applicationVersion pEngineName engineVersion apiVersion) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) pApplicationName
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3)) applicationVersion
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) pEngineName
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4)) engineVersion
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4)) apiVersion


-- | Structure containing callback function pointers for memory allocation..
-- Just ["A pointer-sized variable that the sole use of the application.","A pointer to a function that is called to allocate host memory.","A pointer to a function that is called to resize an existing host memory allocation.","A pointer to a function that is called to free an existing host memory allocation.","A pointer to a function that is called to make short-lived internal allocations.","A pointer to a function that is called to free internal allocations."]
-- Nothing
-- Nothing
-- 
-- This structure is contains pointers to callback functions that are used to create, reallocate
-- and free host memory allocations on behalf of a Vulkan implementation. The pname:pUserData
-- member of the structure is passed to each of the callback functions when they are called.
-- It is otherwise not accessed by the Vulkan implementation and its intended use is that the
-- host application use it to store state information related to memory allocation.
--
-- include::../validity/structs/VkAllocationCallbacks.txt[]
--
-- include::footer.txt[]
-- 
-- == Validaty
-- * pname:pfnAllocation must: be a pointer to a valid user-defined PFN_vkAllocationFunction
-- * pname:pfnReallocation must: be a pointer to a valid user-defined PFN_vkReallocationFunction
-- * pname:pfnFree must: be a pointer to a valid user-defined PFN_vkFreeFunction
-- * If either of pname:pfnInternalAllocation or pname:pfnInternalFree is not `NULL`, both must: be valid callbacks
data VkAllocationCallbacks = VkAllocationCallbacks
  { vac_pUserData :: !(Ptr ())
  -- ^ `pUserData`  Can be `nullPtr`.
  , vac_pfnAllocation :: !PFN_vkAllocationFunction
  -- ^ `pfnAllocation` 
  , vac_pfnReallocation :: !PFN_vkReallocationFunction
  -- ^ `pfnReallocation` 
  , vac_pfnFree :: !PFN_vkFreeFunction
  -- ^ `pfnFree` 
  , vac_pfnInternalAllocation :: !PFN_vkInternalAllocationNotification
  -- ^ `pfnInternalAllocation`  Can be `nullPtr`.
  , vac_pfnInternalFree :: !PFN_vkInternalFreeNotification
  -- ^ `pfnInternalFree`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkAllocationCallbacks where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 0 + sizeOf (undefined :: CSize) * 6
  peek ptr = do
    pUserData <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pfnAllocation <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    pfnReallocation <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    pfnFree <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3))
    pfnInternalAllocation <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 4))
    pfnInternalFree <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 5))
    return $ VkAllocationCallbacks pUserData pfnAllocation pfnReallocation pfnFree pfnInternalAllocation pfnInternalFree
  poke ptr (VkAllocationCallbacks pUserData pfnAllocation pfnReallocation pfnFree pfnInternalAllocation pfnInternalFree) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) pUserData
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pfnAllocation
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) pfnReallocation
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3)) pfnFree
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 4)) pfnInternalAllocation
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 5)) pfnInternalFree


-- | Man not found. VkDeviceQueueCreateInfo
-- 
-- == Validaty
-- * pname:queueFamilyIndex must: be less than pname:pQueueFamilyPropertyCount returned by fname:vkGetPhysicalDeviceQueueFamilyProperties
-- * pname:queueCount must: be less than or equal to the pname:queueCount member of the sname:VkQueueFamilyProperties structure, as returned by fname:vkGetPhysicalDeviceQueueFamilyProperties in the pname:pQueueFamilyProperties[pname:queueFamilyIndex]
-- * Each element of pname:pQueuePriorities must: be between `0.0` and `1.0` inclusive
data VkDeviceQueueCreateInfo = VkDeviceQueueCreateInfo
  { vdqci_sType :: !VkStructureType
  -- ^ `sType` 
  , vdqci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdqci_flags :: !VkDeviceQueueCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vdqci_queueFamilyIndex :: !Word32
  -- ^ `queueFamilyIndex` 
  , vdqci_queueCount :: !Word32
  -- ^ `queueCount` 
  , vdqci_pQueuePriorities :: !(Ptr Float)
  -- ^ `pQueuePriorities`  Const. Length: queueCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDeviceQueueCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    queueFamilyIndex <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    queueCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    pQueuePriorities <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    return $ VkDeviceQueueCreateInfo sType pNext flags queueFamilyIndex queueCount pQueuePriorities
  poke ptr (VkDeviceQueueCreateInfo sType pNext flags queueFamilyIndex queueCount pQueuePriorities) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) queueFamilyIndex
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) queueCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) pQueuePriorities


-- | Man not found. VkDeviceCreateInfo
-- 
-- == Validaty
-- * Any given element of pname:ppEnabledLayerNames must: be the name of a layer present on the system, exactly matching a string returned in the sname:VkLayerProperties structure by fname:vkEnumerateDeviceLayerProperties
-- * Any given element of pname:ppEnabledExtensionNames must: be the name of an extension present on the system, exactly matching a string returned in the sname:VkExtensionProperties structure by fname:vkEnumerateDeviceExtensionProperties
-- * If an extension listed in pname:ppEnabledExtensionNames is provided as part of a layer, then both the layer and extension must: be enabled to enable that extension
-- * The pname:queueFamilyIndex member of any given element of pname:pQueueCreateInfos must: be unique within pname:pQueueCreateInfos
data VkDeviceCreateInfo = VkDeviceCreateInfo
  { vdci_sType :: !VkStructureType
  -- ^ `sType` 
  , vdci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdci_flags :: !VkDeviceCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vdci_queueCreateInfoCount :: !Word32
  -- ^ `queueCreateInfoCount` 
  , vdci_pQueueCreateInfos :: !(Ptr VkDeviceQueueCreateInfo)
  -- ^ `pQueueCreateInfos`  Const. Length: queueCreateInfoCount.
  , vdci_enabledLayerCount :: !Word32
  -- ^ `enabledLayerCount`  Can be `nullPtr`.
  , vdci_ppEnabledLayerNames :: !(Ptr (Ptr CChar))
  -- ^ `ppEnabledLayerNames`  Const. Length: enabledLayerCount,null-terminated.
  , vdci_enabledExtensionCount :: !Word32
  -- ^ `enabledExtensionCount`  Can be `nullPtr`.
  , vdci_ppEnabledExtensionNames :: !(Ptr (Ptr CChar))
  -- ^ `ppEnabledExtensionNames`  Const. Length: enabledExtensionCount,null-terminated.
  , vdci_pEnabledFeatures :: !(Ptr VkPhysicalDeviceFeatures)
  -- ^ `pEnabledFeatures`  Can be `nullPtr`. Const.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDeviceCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 6
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    queueCreateInfoCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pQueueCreateInfos <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    enabledLayerCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    ppEnabledLayerNames <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    enabledExtensionCount <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    ppEnabledExtensionNames <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4))
    pEnabledFeatures <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 5))
    return $ VkDeviceCreateInfo sType pNext flags queueCreateInfoCount pQueueCreateInfos enabledLayerCount ppEnabledLayerNames enabledExtensionCount ppEnabledExtensionNames pEnabledFeatures
  poke ptr (VkDeviceCreateInfo sType pNext flags queueCreateInfoCount pQueueCreateInfos enabledLayerCount ppEnabledLayerNames enabledExtensionCount ppEnabledExtensionNames pEnabledFeatures) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) queueCreateInfoCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) pQueueCreateInfos
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) enabledLayerCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) ppEnabledLayerNames
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) enabledExtensionCount
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4)) ppEnabledExtensionNames
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 5)) pEnabledFeatures


-- | Man not found. VkInstanceCreateInfo
-- 
-- == Validaty
-- * Any given element of pname:ppEnabledLayerNames must: be the name of a layer present on the system, exactly matching a string returned in the sname:VkLayerProperties structure by fname:vkEnumerateInstanceLayerProperties
-- * Any given element of pname:ppEnabledExtensionNames must: be the name of an extension present on the system, exactly matching a string returned in the sname:VkExtensionProperties structure by fname:vkEnumerateInstanceExtensionProperties
-- * If an extension listed in pname:ppEnabledExtensionNames is provided as part of a layer, then both the layer and extension must: be enabled to enable that extension
data VkInstanceCreateInfo = VkInstanceCreateInfo
  { vulkan_sType :: !VkStructureType
  -- ^ `sType` 
  , vulkan_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vulkan_flags :: !VkInstanceCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vulkan_pApplicationInfo :: !(Ptr VkApplicationInfo)
  -- ^ `pApplicationInfo`  Can be `nullPtr`. Const.
  , vulkan_enabledLayerCount :: !Word32
  -- ^ `enabledLayerCount`  Can be `nullPtr`.
  , vulkan_ppEnabledLayerNames :: !(Ptr (Ptr CChar))
  -- ^ `ppEnabledLayerNames`  Const. Length: enabledLayerCount,null-terminated.
  , vulkan_enabledExtensionCount :: !Word32
  -- ^ `enabledExtensionCount`  Can be `nullPtr`.
  , vulkan_ppEnabledExtensionNames :: !(Ptr (Ptr CChar))
  -- ^ `ppEnabledExtensionNames`  Const. Length: enabledExtensionCount,null-terminated.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkInstanceCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 5
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    pApplicationInfo <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    enabledLayerCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    ppEnabledLayerNames <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    enabledExtensionCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4))
    ppEnabledExtensionNames <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    return $ VkInstanceCreateInfo sType pNext flags pApplicationInfo enabledLayerCount ppEnabledLayerNames enabledExtensionCount ppEnabledExtensionNames
  poke ptr (VkInstanceCreateInfo sType pNext flags pApplicationInfo enabledLayerCount ppEnabledLayerNames enabledExtensionCount ppEnabledExtensionNames) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) pApplicationInfo
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) enabledLayerCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) ppEnabledLayerNames
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4)) enabledExtensionCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) ppEnabledExtensionNames


-- | Structure providing information about a queue family..
-- Just ["Capabilities of the queues in this queue family (see elink:VkQueueFlags for more detail).","Number of queues in this queue family.","Tells whether queues in this queue family support timestamps."]
-- Nothing
-- Just ["flink:vkGetPhysicalDeviceQueueFamilyProperties, elink:VkQueueFlags"]
-- 
-- The properties of queue families available in this structure can be retrieved using
-- flink:vkGetPhysicalDeviceQueueFamilyProperties.
--
-- include::../validity/structs/VkQueueFamilyProperties.txt[]
-- 
data VkQueueFamilyProperties = VkQueueFamilyProperties
  { vqfp_queueFlags :: !VkQueueFlags
  -- ^ `queueFlags`  Can be `nullPtr`.
  , vqfp_queueCount :: !Word32
  -- ^ `queueCount` 
  , vqfp_timestampValidBits :: !Word32
  -- ^ `timestampValidBits` 
  , vqfp_minImageTransferGranularity :: !VkExtent3D
  -- ^ `minImageTransferGranularity` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkQueueFamilyProperties where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 24 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    queueFlags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    queueCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    timestampValidBits <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    minImageTransferGranularity <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    return $ VkQueueFamilyProperties queueFlags queueCount timestampValidBits minImageTransferGranularity
  poke ptr (VkQueueFamilyProperties queueFlags queueCount timestampValidBits minImageTransferGranularity) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) queueFlags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) queueCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) timestampValidBits
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) minImageTransferGranularity


-- | Man not found. VkPhysicalDeviceMemoryProperties
-- 
data VkPhysicalDeviceMemoryProperties = VkPhysicalDeviceMemoryProperties
  { vpdmp_memoryTypeCount :: !Word32
  -- ^ `memoryTypeCount` 
  , vpdmp_memoryTypes :: !(V 32 VkMemoryType)
  -- ^ `memoryTypes`  Max: VK_MAX_MEMORY_TYPES.
  , vpdmp_memoryHeapCount :: !Word32
  -- ^ `memoryHeapCount` 
  , vpdmp_memoryHeaps :: !(V 16 VkMemoryHeap)
  -- ^ `memoryHeaps`  Max: VK_MAX_MEMORY_HEAPS.
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkPhysicalDeviceMemoryProperties where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 456 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    memoryTypeCount <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    memoryTypes <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    memoryHeapCount <- peek (plusPtr ptr (260 + sizeOf (undefined :: CSize) * 0))
    memoryHeaps <- peek (plusPtr ptr (264 + sizeOf (undefined :: CSize) * 0))
    return $ VkPhysicalDeviceMemoryProperties memoryTypeCount memoryTypes memoryHeapCount memoryHeaps
  poke ptr (VkPhysicalDeviceMemoryProperties memoryTypeCount memoryTypes memoryHeapCount memoryHeaps) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) memoryTypeCount
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) memoryTypes
    poke (plusPtr ptr (260 + sizeOf (undefined :: CSize) * 0)) memoryHeapCount
    poke (plusPtr ptr (264 + sizeOf (undefined :: CSize) * 0)) memoryHeaps


-- | Structure containing parameters of a memory allocation..
-- Just []
-- Nothing
-- Nothing
-- 
-- include::../validity/structs/VkMemoryAllocateInfo.txt[]
--
-- include::footer.txt[]
-- 
-- == Validaty
-- * pname:allocationSize must: be less than or equal to the amount of memory available to the sname:VkMemoryHeap specified by pname:memoryTypeIndex and the calling command's sname:VkDevice
-- * pname:allocationSize must: be greater than `0`
data VkMemoryAllocateInfo = VkMemoryAllocateInfo
  { vmai_sType :: !VkStructureType
  -- ^ `sType` 
  , vmai_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vmai_allocationSize :: !VkDeviceSize
  -- ^ `allocationSize` 
  , vmai_memoryTypeIndex :: !Word32
  -- ^ `memoryTypeIndex` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkMemoryAllocateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    allocationSize <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    memoryTypeIndex <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    return $ VkMemoryAllocateInfo sType pNext allocationSize memoryTypeIndex
  poke ptr (VkMemoryAllocateInfo sType pNext allocationSize memoryTypeIndex) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) allocationSize
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) memoryTypeIndex


-- | Man not found. VkMemoryRequirements
-- 
data VkMemoryRequirements = VkMemoryRequirements
  { vmr_size :: !VkDeviceSize
  -- ^ `size` 
  , vmr_alignment :: !VkDeviceSize
  -- ^ `alignment` 
  , vmr_memoryTypeBits :: !Word32
  -- ^ `memoryTypeBits` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkMemoryRequirements where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    size <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    alignment <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    memoryTypeBits <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    return $ VkMemoryRequirements size alignment memoryTypeBits
  poke ptr (VkMemoryRequirements size alignment memoryTypeBits) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) size
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) alignment
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) memoryTypeBits


-- | Man not found. VkSparseImageFormatProperties
-- 
data VkSparseImageFormatProperties = VkSparseImageFormatProperties
  { vsifp_aspectMask :: !VkImageAspectFlags
  -- ^ `aspectMask`  Can be `nullPtr`.
  , vsifp_imageGranularity :: !VkExtent3D
  -- ^ `imageGranularity` 
  , vsifp_flags :: !VkSparseImageFormatFlags
  -- ^ `flags`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkSparseImageFormatProperties where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    aspectMask <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    imageGranularity <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    flags <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    return $ VkSparseImageFormatProperties aspectMask imageGranularity flags
  poke ptr (VkSparseImageFormatProperties aspectMask imageGranularity flags) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) aspectMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) imageGranularity
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) flags


-- | Man not found. VkSparseImageMemoryRequirements
-- 
data VkSparseImageMemoryRequirements = VkSparseImageMemoryRequirements
  { vsimr_formatProperties :: !VkSparseImageFormatProperties
  -- ^ `formatProperties` 
  , vsimr_imageMipTailFirstLod :: !Word32
  -- ^ `imageMipTailFirstLod` 
  , vsimr_imageMipTailSize :: !VkDeviceSize
  -- ^ `imageMipTailSize` 
  , vsimr_imageMipTailOffset :: !VkDeviceSize
  -- ^ `imageMipTailOffset` 
  , vsimr_imageMipTailStride :: !VkDeviceSize
  -- ^ `imageMipTailStride` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkSparseImageMemoryRequirements where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 48 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    formatProperties <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    imageMipTailFirstLod <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0))
    imageMipTailSize <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0))
    imageMipTailOffset <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0))
    imageMipTailStride <- peek (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0))
    return $ VkSparseImageMemoryRequirements formatProperties imageMipTailFirstLod imageMipTailSize imageMipTailOffset imageMipTailStride
  poke ptr (VkSparseImageMemoryRequirements formatProperties imageMipTailFirstLod imageMipTailSize imageMipTailOffset imageMipTailStride) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) formatProperties
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0)) imageMipTailFirstLod
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0)) imageMipTailSize
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0)) imageMipTailOffset
    poke (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0)) imageMipTailStride


-- | Man not found. VkMemoryType
-- 
data VkMemoryType = VkMemoryType
  { vmt_propertyFlags :: !VkMemoryPropertyFlags
  -- ^ `propertyFlags`  Can be `nullPtr`.
  , vmt_heapIndex :: !Word32
  -- ^ `heapIndex` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkMemoryType where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    propertyFlags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    heapIndex <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    return $ VkMemoryType propertyFlags heapIndex
  poke ptr (VkMemoryType propertyFlags heapIndex) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) propertyFlags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) heapIndex


-- | Man not found. VkMemoryHeap
-- 
data VkMemoryHeap = VkMemoryHeap
  { vmh_size :: !VkDeviceSize
  -- ^ `size` 
  , vmh_flags :: !VkMemoryHeapFlags
  -- ^ `flags`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkMemoryHeap where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    size <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    flags <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkMemoryHeap size flags
  poke ptr (VkMemoryHeap size flags) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) size
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) flags


-- | Man not found. VkMappedMemoryRange
-- 
-- == Validaty
-- * pname:memory must: currently be mapped
-- * If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:offset and pname:size must: specify a range contained within the currently mapped range of pname:memory
-- * If pname:size is equal to ename:VK_WHOLE_SIZE, pname:offset must: be within the currently mapped range of pname:memory
-- * pname:offset must: be a multiple of sname:VkPhysicalDeviceLimits::pname:nonCoherentAtomSize
-- * If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be a multiple of sname:VkPhysicalDeviceLimits::pname:nonCoherentAtomSize
data VkMappedMemoryRange = VkMappedMemoryRange
  { vmmr_sType :: !VkStructureType
  -- ^ `sType` 
  , vmmr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vmmr_memory :: !VkDeviceMemory
  -- ^ `memory` 
  , vmmr_offset :: !VkDeviceSize
  -- ^ `offset` 
  , vmmr_size :: !VkDeviceSize
  -- ^ `size` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkMappedMemoryRange where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 24 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    memory <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    offset <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    size <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    return $ VkMappedMemoryRange sType pNext memory offset size
  poke ptr (VkMappedMemoryRange sType pNext memory offset size) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) memory
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) offset
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) size


-- | Man not found. VkFormatProperties
-- 
data VkFormatProperties = VkFormatProperties
  { vfp_linearTilingFeatures :: !VkFormatFeatureFlags
  -- ^ `linearTilingFeatures`  Can be `nullPtr`.
  , vfp_optimalTilingFeatures :: !VkFormatFeatureFlags
  -- ^ `optimalTilingFeatures`  Can be `nullPtr`.
  , vfp_bufferFeatures :: !VkFormatFeatureFlags
  -- ^ `bufferFeatures`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkFormatProperties where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    linearTilingFeatures <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    optimalTilingFeatures <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    bufferFeatures <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkFormatProperties linearTilingFeatures optimalTilingFeatures bufferFeatures
  poke ptr (VkFormatProperties linearTilingFeatures optimalTilingFeatures bufferFeatures) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) linearTilingFeatures
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) optimalTilingFeatures
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) bufferFeatures


-- | Man not found. VkImageFormatProperties
-- 
data VkImageFormatProperties = VkImageFormatProperties
  { vifp_maxExtent :: !VkExtent3D
  -- ^ `maxExtent` 
  , vifp_maxMipLevels :: !Word32
  -- ^ `maxMipLevels` 
  , vifp_maxArrayLayers :: !Word32
  -- ^ `maxArrayLayers` 
  , vifp_sampleCounts :: !VkSampleCountFlags
  -- ^ `sampleCounts`  Can be `nullPtr`.
  , vifp_maxResourceSize :: !VkDeviceSize
  -- ^ `maxResourceSize` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkImageFormatProperties where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 32 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    maxExtent <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    maxMipLevels <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    maxArrayLayers <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    sampleCounts <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0))
    maxResourceSize <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0))
    return $ VkImageFormatProperties maxExtent maxMipLevels maxArrayLayers sampleCounts maxResourceSize
  poke ptr (VkImageFormatProperties maxExtent maxMipLevels maxArrayLayers sampleCounts maxResourceSize) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) maxExtent
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) maxMipLevels
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) maxArrayLayers
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0)) sampleCounts
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0)) maxResourceSize


-- | Man not found. VkDescriptorBufferInfo
-- 
-- == Validaty
-- * pname:offset must: be less than the size of pname:buffer
-- * If pname:range is not equal to ename:VK_WHOLE_SIZE, pname:range must: be greater than `0`
-- * If pname:range is not equal to ename:VK_WHOLE_SIZE, pname:range must: be less than or equal to the size of pname:buffer minus pname:offset
data VkDescriptorBufferInfo = VkDescriptorBufferInfo
  { vdbi_buffer :: !VkBuffer
  -- ^ `buffer` 
  , vdbi_offset :: !VkDeviceSize
  -- ^ `offset` 
  , vdbi_range :: !VkDeviceSize
  -- ^ `range` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDescriptorBufferInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 24 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    buffer <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    offset <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    range <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    return $ VkDescriptorBufferInfo buffer offset range
  poke ptr (VkDescriptorBufferInfo buffer offset range) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) buffer
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) offset
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) range


-- | Man not found. VkDescriptorImageInfo
-- 
data VkDescriptorImageInfo = VkDescriptorImageInfo
  { vdii_sampler :: !VkSampler
  -- ^ `sampler`  Noautovalidity.
  , vdii_imageView :: !VkImageView
  -- ^ `imageView`  Noautovalidity.
  , vdii_imageLayout :: !VkImageLayout
  -- ^ `imageLayout`  Noautovalidity.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDescriptorImageInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    sampler <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    imageView <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    imageLayout <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    return $ VkDescriptorImageInfo sampler imageView imageLayout
  poke ptr (VkDescriptorImageInfo sampler imageView imageLayout) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sampler
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) imageView
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) imageLayout


-- | Structure specifying the parameters of a descriptor set write operation..
-- Just ["Structure type. Must be ename:VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET.","Pointer to next structure in the structure chain when applicable.","Destination descriptor set to write the descriptor data to.","Binding within the descriptor set to start the update from.","Array element of the binding to start the update from.","Number of descriptors to write to the descriptor set.","Type of descriptors to write to the descriptor set.","A pointer to an array of pname:descriptorCount slink:VkDescriptorImageInfo\nstructures specifying the source of the descriptor data to write to the\ndescriptor set for images.","A pointer to an array of pname:descriptorCount slink:VkDescriptorBufferInfo\nstructures specifying the source of the descriptor data to write to the\ndescriptor set for buffers.","A pointer to an array of basetype:VkBufferView handles used when binding\ntexel buffers into a the descriptor set."]
-- Nothing
-- Just ["flink:vkUpdateDescriptorSets, slink:VkDescriptorImageInfo, slink:VkDescriptorBufferInfo, elink:VkDescriptorType"]
-- 
-- This structure specifies information about the descriptors to be written to
-- a descriptor set using the fname:vkUpdateDescriptorSets command.
--
-- When writing data to descriptor sets, the pname:pImageInfo, pname:pBufferInfo
-- or pname:pTexelBufferView parameters of fname:vkUpdateDescriptorSets point
-- to pname:descriptorCount instances of data structures, each instance
-- specifying the source of the descriptor data to be written. Which of these
-- parameters is used depends on the value of pname:descriptorType.
--
-- Each instance of the selected array allows writing pname:descriptorCount
-- descriptors of type pname:descriptorType to the destination descriptor
-- set specified by pname:dstSet starting from the array element index
-- pname:dstArrayElement of the pname:dstBinding binding.
--
-- If pname:descriptorCount is greater than the number of descriptors in the
-- specified binding starting from the specified array element index then
-- subsequent descriptors are written to the next binding starting from its
-- first array element. This allows updating multiple subsequent bindings with
-- a single instance of this structure as long as the descriptor type of those
-- bindings match.
--
-- Attempting to write descriptors of incompatible type to any binding of a
-- descriptor set may result in undefined behavior.
--
-- include::../validity/structs/VkWriteDescriptorSet.txt[]
-- 
-- == Validaty
-- * pname:dstBinding must: be a valid binding point within pname:dstSet
-- * pname:descriptorType must: match the type of pname:dstBinding within pname:dstSet
-- * The sum of pname:dstArrayElement and pname:descriptorCount must: be less than or equal to the number of array elements in the descriptor set binding specified by pname:dstBinding, and all applicable consecutive bindings, as described by <<descriptorsets-updates-consecutive>>
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_SAMPLER, ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, ename:VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE, ename:VK_DESCRIPTOR_TYPE_STORAGE_IMAGE or ename:VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT, pname:pImageInfo must: be a pointer to an array of pname:descriptorCount valid sname:VkDescriptorImageInfo structures
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER or ename:VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER, pname:pTexelBufferView must: be a pointer to an array of pname:descriptorCount valid sname:VkBufferView handles
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC or ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, pname:pBufferInfo must: be a pointer to an array of pname:descriptorCount valid sname:VkDescriptorBufferInfo structures
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_SAMPLER or ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, and pname:dstSet was not created with a layout that included immutable samplers for pname:dstBinding with pname:descriptorType, the pname:sampler member of any given element of pname:pImageInfo must: be a valid sname:VkSampler object
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, ename:VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE, ename:VK_DESCRIPTOR_TYPE_STORAGE_IMAGE or ename:VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT, the pname:imageView and pname:imageLayout members of any given element of pname:pImageInfo must: be a valid sname:VkImageView and elink:VkImageLayout, respectively
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER or ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, the pname:offset member of any given element of pname:pBufferInfo must: be a multiple of sname:VkPhysicalDeviceLimits::pname:minUniformBufferOffsetAlignment
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER or ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, the pname:offset member of any given element of pname:pBufferInfo must: be a multiple of sname:VkPhysicalDeviceLimits::pname:minStorageBufferOffsetAlignment
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER or ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, the pname:buffer member of any given element of pname:pBufferInfo must: have been created with ename:VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT set
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER or ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, the pname:buffer member of any given element of pname:pBufferInfo must: have been created with ename:VK_BUFFER_USAGE_STORAGE_BUFFER_BIT set
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER or ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, the pname:range member of any given element of pname:pBufferInfo must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxUniformBufferRange
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER or ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, the pname:range member of any given element of pname:pBufferInfo must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxStorageBufferRange
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER, the sname:VkBuffer that any given element of pname:pTexelBufferView was created from must: have been created with ename:VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT set
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER, the sname:VkBuffer that any given element of pname:pTexelBufferView was created from must: have been created with ename:VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT set
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_STORAGE_IMAGE or ename:VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT, the pname:imageView member of any given element of pname:pImageInfo must: have been created with the identity swizzle
data VkWriteDescriptorSet = VkWriteDescriptorSet
  { vwds_sType :: !VkStructureType
  -- ^ `sType` 
  , vwds_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vwds_dstSet :: !VkDescriptorSet
  -- ^ `dstSet` 
  , vwds_dstBinding :: !Word32
  -- ^ `dstBinding` 
  , vwds_dstArrayElement :: !Word32
  -- ^ `dstArrayElement` 
  , vwds_descriptorCount :: !Word32
  -- ^ `descriptorCount` 
  , vwds_descriptorType :: !VkDescriptorType
  -- ^ `descriptorType` 
  , vwds_pImageInfo :: !(Ptr VkDescriptorImageInfo)
  -- ^ `pImageInfo`  Const. Length: descriptorCount. Noautovalidity.
  , vwds_pBufferInfo :: !(Ptr VkDescriptorBufferInfo)
  -- ^ `pBufferInfo`  Const. Length: descriptorCount. Noautovalidity.
  , vwds_pTexelBufferView :: !(Ptr VkBufferView)
  -- ^ `pTexelBufferView`  Const. Length: descriptorCount. Noautovalidity.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkWriteDescriptorSet where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 6
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    dstSet <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    dstBinding <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    dstArrayElement <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    descriptorCount <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    descriptorType <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 2))
    pImageInfo <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3))
    pBufferInfo <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 4))
    pTexelBufferView <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 5))
    return $ VkWriteDescriptorSet sType pNext dstSet dstBinding dstArrayElement descriptorCount descriptorType pImageInfo pBufferInfo pTexelBufferView
  poke ptr (VkWriteDescriptorSet sType pNext dstSet dstBinding dstArrayElement descriptorCount descriptorType pImageInfo pBufferInfo pTexelBufferView) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) dstSet
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) dstBinding
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) dstArrayElement
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) descriptorCount
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 2)) descriptorType
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3)) pImageInfo
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 4)) pBufferInfo
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 5)) pTexelBufferView


-- | Man not found. VkCopyDescriptorSet
-- 
-- == Validaty
-- * pname:srcBinding must: be a valid binding within pname:srcSet
-- * The sum of pname:srcArrayElement and pname:descriptorCount must: be less than or equal to the number of array elements in the descriptor set binding specified by pname:srcBinding, and all applicable consecutive bindings, as described by <<descriptorsets-updates-consecutive>>
-- * pname:dstBinding must: be a valid binding within pname:dstSet
-- * The sum of pname:dstArrayElement and pname:descriptorCount must: be less than or equal to the number of array elements in the descriptor set binding specified by pname:dstBinding, and all applicable consecutive bindings, as described by <<descriptorsets-updates-consecutive>>
-- * If pname:srcSet is equal to pname:dstSet, then the source and destination ranges of descriptors mustnot: overlap, where the ranges may: include array elements from consecutive bindings as described by <<descriptorsets-updates-consecutive>>
data VkCopyDescriptorSet = VkCopyDescriptorSet
  { vcds_sType :: !VkStructureType
  -- ^ `sType` 
  , vcds_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vcds_srcSet :: !VkDescriptorSet
  -- ^ `srcSet` 
  , vcds_srcBinding :: !Word32
  -- ^ `srcBinding` 
  , vcds_srcArrayElement :: !Word32
  -- ^ `srcArrayElement` 
  , vcds_dstSet :: !VkDescriptorSet
  -- ^ `dstSet` 
  , vcds_dstBinding :: !Word32
  -- ^ `dstBinding` 
  , vcds_dstArrayElement :: !Word32
  -- ^ `dstArrayElement` 
  , vcds_descriptorCount :: !Word32
  -- ^ `descriptorCount` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkCopyDescriptorSet where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 36 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    srcSet <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    srcBinding <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    srcArrayElement <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    dstSet <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    dstBinding <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 2))
    dstArrayElement <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 2))
    descriptorCount <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 2))
    return $ VkCopyDescriptorSet sType pNext srcSet srcBinding srcArrayElement dstSet dstBinding dstArrayElement descriptorCount
  poke ptr (VkCopyDescriptorSet sType pNext srcSet srcBinding srcArrayElement dstSet dstBinding dstArrayElement descriptorCount) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) srcSet
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) srcBinding
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) srcArrayElement
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) dstSet
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 2)) dstBinding
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 2)) dstArrayElement
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 2)) descriptorCount


-- | Structure specifying the parameters of a newly created buffer object..
-- Just ["Structure type. Must be ename:VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO.","Pointer to next structure in the structure chain when applicable.","Size of the buffer in bytes.","Allowed usages of the buffer (see elink:VkBufferUsageFlags for more detail).","Other properties of the buffer (see elink:VkBufferCreateFlags for more detail).","Sharing mode used for the buffer (see elink:VkSharingMode for more detail).","Number of queue families that can access the buffer in case\npname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT.","Array of pname:queueFamilyIndexCount queue family indices specifying the\nset of queue families that can access the buffer in case\npname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT."]
-- Nothing
-- Just ["flink:vkCreateBuffer"]
-- 
-- This structure is used to specify the parameters of buffer objects created using
-- flink:vkCreateBuffer.
--
-- include::../validity/structs/VkBufferCreateInfo.txt[]
-- 
-- == Validaty
-- * pname:size must: be greater than `0`
-- * If pname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:pQueueFamilyIndices must: be a pointer to an array of pname:queueFamilyIndexCount basetype:uint32_t values
-- * If pname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:queueFamilyIndexCount must: be greater than `1`
-- * If the <<features-features-sparseBinding,sparse bindings>> feature is not enabled, pname:flags mustnot: contain ename:VK_BUFFER_CREATE_SPARSE_BINDING_BIT
-- * If the <<features-features-sparseResidencyBuffer,sparse buffer residency>> feature is not enabled, pname:flags mustnot: contain ename:VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
-- * If the <<features-features-sparseResidencyAliased,sparse aliased residency>> feature is not enabled, pname:flags mustnot: contain ename:VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
-- * If pname:flags contains ename:VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT or ename:VK_BUFFER_CREATE_SPARSE_ALIASED_BIT, it must: also contain ename:VK_BUFFER_CREATE_SPARSE_BINDING_BIT
data VkBufferCreateInfo = VkBufferCreateInfo
  { vbci_sType :: !VkStructureType
  -- ^ `sType` 
  , vbci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vbci_flags :: !VkBufferCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vbci_size :: !VkDeviceSize
  -- ^ `size` 
  , vbci_usage :: !VkBufferUsageFlags
  -- ^ `usage` 
  , vbci_sharingMode :: !VkSharingMode
  -- ^ `sharingMode` 
  , vbci_queueFamilyIndexCount :: !Word32
  -- ^ `queueFamilyIndexCount`  Can be `nullPtr`.
  , vbci_pQueueFamilyIndices :: !(Ptr Word32)
  -- ^ `pQueueFamilyIndices`  Const. Length: queueFamilyIndexCount. Noautovalidity.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkBufferCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    size <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    usage <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    sharingMode <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    queueFamilyIndexCount <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3))
    pQueueFamilyIndices <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3))
    return $ VkBufferCreateInfo sType pNext flags size usage sharingMode queueFamilyIndexCount pQueueFamilyIndices
  poke ptr (VkBufferCreateInfo sType pNext flags size usage sharingMode queueFamilyIndexCount pQueueFamilyIndices) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) size
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) usage
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) sharingMode
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3)) queueFamilyIndexCount
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3)) pQueueFamilyIndices


-- | Man not found. VkBufferViewCreateInfo
-- 
-- == Validaty
-- * pname:offset must: be less than the size of pname:buffer
-- * pname:offset must: be a multiple of sname:VkPhysicalDeviceLimits::pname:minTexelBufferOffsetAlignment
-- * If pname:range is not equal to ename:VK_WHOLE_SIZE:
--                         ** pname:range must: be greater than `0`
--                         ** pname:range must: be a multiple of the element size of pname:format
--                         ** pname:range divided by the size of an element of pname:format, must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxTexelBufferElements
--                         ** the sum of pname:offset and pname:range must: be less than or equal to the size of pname:buffer
-- * pname:buffer must: have been created with a pname:usage value containing at least one of ename:VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT or ename:VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
-- * If pname:buffer was created with pname:usage containing ename:VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT, pname:format must: be supported for uniform texel buffers, as specified by the ename:VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT flag in sname:VkFormatProperties::pname:bufferFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:buffer was created with pname:usage containing ename:VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT, pname:format must: be supported for storage texel buffers, as specified by the ename:VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT flag in sname:VkFormatProperties::pname:bufferFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
data VkBufferViewCreateInfo = VkBufferViewCreateInfo
  { vbvci_sType :: !VkStructureType
  -- ^ `sType` 
  , vbvci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vbvci_flags :: !VkBufferViewCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vbvci_buffer :: !VkBuffer
  -- ^ `buffer` 
  , vbvci_format :: !VkFormat
  -- ^ `format` 
  , vbvci_offset :: !VkDeviceSize
  -- ^ `offset` 
  , vbvci_range :: !VkDeviceSize
  -- ^ `range` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkBufferViewCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 28 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    buffer <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    format <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    offset <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    range <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3))
    return $ VkBufferViewCreateInfo sType pNext flags buffer format offset range
  poke ptr (VkBufferViewCreateInfo sType pNext flags buffer format offset range) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) buffer
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) format
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) offset
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3)) range


-- | Man not found. VkImageSubresource
-- 
-- == Validaty
-- * pname:mipLevel must: be less than the pname:mipLevels specified in slink:VkImageCreateInfo when the image was created
-- * pname:arrayLayer must: be less than the pname:arrayLayers specified in slink:VkImageCreateInfo when the image was created
data VkImageSubresource = VkImageSubresource
  { vis_aspectMask :: !VkImageAspectFlags
  -- ^ `aspectMask` 
  , vis_mipLevel :: !Word32
  -- ^ `mipLevel` 
  , vis_arrayLayer :: !Word32
  -- ^ `arrayLayer` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkImageSubresource where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    aspectMask <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    mipLevel <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    arrayLayer <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkImageSubresource aspectMask mipLevel arrayLayer
  poke ptr (VkImageSubresource aspectMask mipLevel arrayLayer) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) aspectMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) mipLevel
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) arrayLayer


-- | Man not found. VkImageSubresourceLayers
-- 
-- == Validaty
-- * If pname:aspectMask contains ename:VK_IMAGE_ASPECT_COLOR_BIT, it mustnot: contain either of ename:VK_IMAGE_ASPECT_DEPTH_BIT or ename:VK_IMAGE_ASPECT_STENCIL_BIT
-- * pname:aspectMask mustnot: contain ename:VK_IMAGE_ASPECT_METADATA_BIT
-- * pname:mipLevel must: be less than the pname:mipLevels specified in slink:VkImageCreateInfo when the image was created
-- * latexmath:[$(baseArrayLayer + layerCount)$] must: be less than or equal to the pname:arrayLayers specified in slink:VkImageCreateInfo when the image was created
data VkImageSubresourceLayers = VkImageSubresourceLayers
  { visl_aspectMask :: !VkImageAspectFlags
  -- ^ `aspectMask` 
  , visl_mipLevel :: !Word32
  -- ^ `mipLevel` 
  , visl_baseArrayLayer :: !Word32
  -- ^ `baseArrayLayer` 
  , visl_layerCount :: !Word32
  -- ^ `layerCount` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkImageSubresourceLayers where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    aspectMask <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    mipLevel <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    baseArrayLayer <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    layerCount <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    return $ VkImageSubresourceLayers aspectMask mipLevel baseArrayLayer layerCount
  poke ptr (VkImageSubresourceLayers aspectMask mipLevel baseArrayLayer layerCount) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) aspectMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) mipLevel
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) baseArrayLayer
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) layerCount


-- | Man not found. VkImageSubresourceRange
-- 
-- == Validaty
-- * If pname:levelCount is not ename:VK_REMAINING_MIP_LEVELS, latexmath:[$(baseMipLevel + levelCount)$] must: be less than or equal to the pname:mipLevels specified in slink:VkImageCreateInfo when the image was created
-- * If pname:layerCount is not ename:VK_REMAINING_ARRAY_LAYERS, latexmath:[$(baseArrayLayer + layerCount)$] must: be less than or equal to the pname:arrayLayers specified in slink:VkImageCreateInfo when the image was created
data VkImageSubresourceRange = VkImageSubresourceRange
  { visr_aspectMask :: !VkImageAspectFlags
  -- ^ `aspectMask` 
  , visr_baseMipLevel :: !Word32
  -- ^ `baseMipLevel` 
  , visr_levelCount :: !Word32
  -- ^ `levelCount` 
  , visr_baseArrayLayer :: !Word32
  -- ^ `baseArrayLayer` 
  , visr_layerCount :: !Word32
  -- ^ `layerCount` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkImageSubresourceRange where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    aspectMask <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    baseMipLevel <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    levelCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    baseArrayLayer <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    layerCount <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    return $ VkImageSubresourceRange aspectMask baseMipLevel levelCount baseArrayLayer layerCount
  poke ptr (VkImageSubresourceRange aspectMask baseMipLevel levelCount baseArrayLayer layerCount) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) aspectMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) baseMipLevel
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) levelCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) baseArrayLayer
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) layerCount


-- | Man not found. VkMemoryBarrier
-- 
data VkMemoryBarrier = VkMemoryBarrier
  { vmb_sType :: !VkStructureType
  -- ^ `sType` 
  , vmb_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vmb_srcAccessMask :: !VkAccessFlags
  -- ^ `srcAccessMask`  Can be `nullPtr`.
  , vmb_dstAccessMask :: !VkAccessFlags
  -- ^ `dstAccessMask`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkMemoryBarrier where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    srcAccessMask <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    dstAccessMask <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    return $ VkMemoryBarrier sType pNext srcAccessMask dstAccessMask
  poke ptr (VkMemoryBarrier sType pNext srcAccessMask dstAccessMask) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) srcAccessMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) dstAccessMask


-- | Structure specifying the parameters of a buffer memory barrier..
-- Just ["Structure type. Must be ename:VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER.","Pointer to next structure in the structure chain when applicable.","Types of writes to the buffer to flush (see elink:VkMemoryOutputFlags for more detail).","Types of reads from the buffer to invalidate (see elink:VkMemoryInputFlags for more detail).","Identifies the source queue family to transfer ownership of the buffer from.\nA value of ename:VK_QUEUE_FAMILY_IGNORED indicates that this member should be ignored.","Identifies the destination queue family to transfer ownership of the buffer to.\nA value of ename:VK_QUEUE_FAMILY_IGNORED indicates that this member should be ignored.","Buffer object the memory barrier applies to.","Byte offset of the sub-range of the buffer the memory barrier applies to.","Size in bytes of the sub-range of the buffer the memory barrier applies to.",""]
-- Nothing
-- Just ["flink:vkCmdPipelineBarrier, flink:vkCmdWaitEvents, slink:VkMemoryBarrier, slink:VkImageMemoryBarrier"]
-- 
-- This structure specifies the parameters of a buffer memory barrier that can be passed in the ptext:ppMemoryBarriers
-- parameter of flink:vkCmdPipelineBarrier and flink:vkCmdWaitEvents.
-- 
-- == Validaty
-- * pname:offset must: be less than the size of pname:buffer
-- * If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be greater than `0`
-- * If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be less than or equal to than the size of pname:buffer minus pname:offset
-- * If pname:buffer was created with a sharing mode of ename:VK_SHARING_MODE_CONCURRENT, pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex must: both be ename:VK_QUEUE_FAMILY_IGNORED
-- * If pname:buffer was created with a sharing mode of ename:VK_SHARING_MODE_EXCLUSIVE, pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex must: either both be ename:VK_QUEUE_FAMILY_IGNORED, or both be a valid queue family (see <<devsandqueues-queueprops>>)
-- * If pname:buffer was created with a sharing mode of ename:VK_SHARING_MODE_EXCLUSIVE, and pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex are valid queue families, at least one of them must: be the same as the family of the queue that will execute this barrier
data VkBufferMemoryBarrier = VkBufferMemoryBarrier
  { vbmb_sType :: !VkStructureType
  -- ^ `sType` 
  , vbmb_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vbmb_srcAccessMask :: !VkAccessFlags
  -- ^ `srcAccessMask`  Can be `nullPtr`.
  , vbmb_dstAccessMask :: !VkAccessFlags
  -- ^ `dstAccessMask`  Can be `nullPtr`.
  , vbmb_srcQueueFamilyIndex :: !Word32
  -- ^ `srcQueueFamilyIndex` 
  , vbmb_dstQueueFamilyIndex :: !Word32
  -- ^ `dstQueueFamilyIndex` 
  , vbmb_buffer :: !VkBuffer
  -- ^ `buffer` 
  , vbmb_offset :: !VkDeviceSize
  -- ^ `offset` 
  , vbmb_size :: !VkDeviceSize
  -- ^ `size` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkBufferMemoryBarrier where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 40 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    srcAccessMask <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    dstAccessMask <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    srcQueueFamilyIndex <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    dstQueueFamilyIndex <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    buffer <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    offset <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 2))
    size <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 2))
    return $ VkBufferMemoryBarrier sType pNext srcAccessMask dstAccessMask srcQueueFamilyIndex dstQueueFamilyIndex buffer offset size
  poke ptr (VkBufferMemoryBarrier sType pNext srcAccessMask dstAccessMask srcQueueFamilyIndex dstQueueFamilyIndex buffer offset size) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) srcAccessMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) dstAccessMask
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) srcQueueFamilyIndex
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) dstQueueFamilyIndex
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) buffer
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 2)) offset
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 2)) size


-- | Structure specifying the parameters of an image memory barrier..
-- Just ["Structure type. Must be ename:VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER.","Pointer to next structure in the structure chain when applicable.","Types of writes to the image to flush (see elink:VkMemoryOutputFlags for more detail).","Types of reads from the image to invalidate (see elink:VkMemoryInputFlags for more detail).","Current layout the image is expected to be in (see elink:VkImageLayout for more detail).","New layout the image should be transferred to (see elink:VkImageLayout for more detail).","Identifies the source queue family to transfer ownership of the image from.\nA value of ename:VK_QUEUE_FAMILY_IGNORED indicates that this member should be ignored.","Identifies the destination queue family to transfer ownership of the image to.\nA value of ename:VK_QUEUE_FAMILY_IGNORED indicates that this member should be ignored.","Image object the memory barrier applies to.","Sub-range of the image the memory barrier applies to."]
-- Nothing
-- Just ["flink:vkCmdPipelineBarrier, flink:vkCmdWaitEvents, slink:VkMemoryBarrier, slink:VkBufferMemoryBarrier"]
-- 
-- This structure specifies the parameters of an image memory barrier that can
-- be passed in the ptext:ppMemBarriers parameter of flink:vkCmdPipelineBarrier
-- and flink:vkCmdWaitEvents.
--
-- include::../validity/structs/VkImageMemoryBarrier.txt[]
-- 
-- == Validaty
-- * pname:oldLayout must: be ename:VK_IMAGE_LAYOUT_UNDEFINED, ename:VK_IMAGE_LAYOUT_PREINITIALIZED or the current layout of the image region affected by the barrier
-- * pname:newLayout mustnot: be ename:VK_IMAGE_LAYOUT_UNDEFINED or ename:VK_IMAGE_LAYOUT_PREINITIALIZED
-- * If pname:image was created with a sharing mode of ename:VK_SHARING_MODE_CONCURRENT, pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex must: both be ename:VK_QUEUE_FAMILY_IGNORED
-- * If pname:image was created with a sharing mode of ename:VK_SHARING_MODE_EXCLUSIVE, pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex must: either both be ename:VK_QUEUE_FAMILY_IGNORED, or both be a valid queue family (see <<devsandqueues-queueprops>>)
-- * If pname:image was created with a sharing mode of ename:VK_SHARING_MODE_EXCLUSIVE, and pname:srcQueueFamilyIndex and pname:dstQueueFamilyIndex are valid queue families, at least one of them must: be the same as the family of the queue that will execute this barrier
-- * pname:subresourceRange must: be a valid image subresource range for the image (see <<resources-image-views>>)
-- * If pname:image has a depth/stencil format with both depth and stencil components, then pname:aspectMask member of pname:subresourceRange must: include both ename:VK_IMAGE_ASPECT_DEPTH_BIT and ename:VK_IMAGE_ASPECT_STENCIL_BIT
-- * If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT set
-- * If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT set
-- * If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT set
-- * If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_SAMPLED_BIT or ename:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT set
-- * If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_SRC_BIT set
-- * If either pname:oldLayout or pname:newLayout is ename:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL then pname:image must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_DST_BIT set
data VkImageMemoryBarrier = VkImageMemoryBarrier
  { vimb_sType :: !VkStructureType
  -- ^ `sType` 
  , vimb_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vimb_srcAccessMask :: !VkAccessFlags
  -- ^ `srcAccessMask`  Can be `nullPtr`.
  , vimb_dstAccessMask :: !VkAccessFlags
  -- ^ `dstAccessMask`  Can be `nullPtr`.
  , vimb_oldLayout :: !VkImageLayout
  -- ^ `oldLayout` 
  , vimb_newLayout :: !VkImageLayout
  -- ^ `newLayout` 
  , vimb_srcQueueFamilyIndex :: !Word32
  -- ^ `srcQueueFamilyIndex` 
  , vimb_dstQueueFamilyIndex :: !Word32
  -- ^ `dstQueueFamilyIndex` 
  , vimb_image :: !VkImage
  -- ^ `image` 
  , vimb_subresourceRange :: !VkImageSubresourceRange
  -- ^ `subresourceRange` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkImageMemoryBarrier where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 44 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    srcAccessMask <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    dstAccessMask <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    oldLayout <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    newLayout <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    srcQueueFamilyIndex <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4))
    dstQueueFamilyIndex <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    image <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4))
    subresourceRange <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4))
    return $ VkImageMemoryBarrier sType pNext srcAccessMask dstAccessMask oldLayout newLayout srcQueueFamilyIndex dstQueueFamilyIndex image subresourceRange
  poke ptr (VkImageMemoryBarrier sType pNext srcAccessMask dstAccessMask oldLayout newLayout srcQueueFamilyIndex dstQueueFamilyIndex image subresourceRange) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) srcAccessMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) dstAccessMask
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) oldLayout
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) newLayout
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4)) srcQueueFamilyIndex
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) dstQueueFamilyIndex
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4)) image
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4)) subresourceRange


-- | Structure specifying the parameters of a newly created image object..
-- Just ["Structure type. Must be ename:VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO.","Pointer to next structure in the structure chain when applicable.","Type of the image (see elink:VkImageType for more detail).","Format of the texels of the image (see elink:VkFormat for more detail).","Width, height, and depth of the image in texels.","Number of mip levels of the image.","Number of layers of the image.","Number of samples of the image.","Image tiling mode of the image (see elink:VkImageTiling for more detail).","Allowed usages of the image (see elink:VkImageUsageFlags for more detail).","Other properties of the image (see elink:VkImageCreateFlags for more detail).","Sharing mode used for the image (see elink:VkSharingMode for more detail).","Number of queue families that can access the image in case\npname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT.","Array of pname:queueFamilyIndexCount queue family indices specifying the\nset of queue families that can access the image in case\npname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT."]
-- Nothing
-- Just ["flink:vkCreateImage"]
-- 
-- This structure is used to specify the parameters of image objects created using
-- flink:vkCreateImage.
--
-- include::../validity/structs/VkImageCreateInfo.txt[]
-- 
-- == Validaty
-- * If pname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:pQueueFamilyIndices must: be a pointer to an array of pname:queueFamilyIndexCount basetype:uint32_t values
-- * If pname:sharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:queueFamilyIndexCount must: be greater than `1`
-- * pname:format mustnot: be ename:VK_FORMAT_UNDEFINED
-- * The pname:width, pname:height, and pname:depth members of pname:extent must: all be greater than `0`
-- * pname:mipLevels must: be greater than `0`
-- * pname:arrayLayers must: be greater than `0`
-- * If pname:imageType is ename:VK_IMAGE_TYPE_1D, pname:extent.width must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageDimension1D, or sname:VkImageFormatProperties::pname:maxExtent.width (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
-- * If pname:imageType is ename:VK_IMAGE_TYPE_2D and pname:flags does not contain ename:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT, pname:extent.width and pname:extent.height must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageDimension2D, or sname:VkImageFormatProperties::pname:maxExtent.width/height (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
-- * If pname:imageType is ename:VK_IMAGE_TYPE_2D and pname:flags contains ename:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT, pname:extent.width and pname:extent.height must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageDimensionCube, or sname:VkImageFormatProperties::pname:maxExtent.width/height (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
-- * If pname:imageType is ename:VK_IMAGE_TYPE_2D and pname:flags contains ename:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT, pname:extent.width and pname:extent.height must: be equal
-- * If pname:imageType is ename:VK_IMAGE_TYPE_3D, pname:extent.width, pname:extent.height and pname:extent.depth must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageDimension3D, or sname:VkImageFormatProperties::pname:maxExtent.width/height/depth (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
-- * If pname:imageType is ename:VK_IMAGE_TYPE_1D, both pname:extent.height and pname:extent.depth must: be `1`
-- * If pname:imageType is ename:VK_IMAGE_TYPE_2D, pname:extent.depth must: be `1`
-- * pname:mipLevels must: be less than or equal to latexmath:[$\lfloor\log_2(\max(\mathit{extent.width}, \mathit{extent.height}, \mathit{extent.depth}))\rfloor + 1$]
-- * If any of pname:extent.width, pname:extent.height or pname:extent.depth are greater than the equivalently named members of sname:VkPhysicalDeviceLimits::pname:maxImageDimension3D, pname:mipLevels must: be less than or equal to sname:VkImageFormatProperties::pname:maxMipLevels (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure)
-- * pname:arrayLayers must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxImageArrayLayers, or sname:VkImageFormatProperties::pname:maxArrayLayers (as returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure) - whichever is higher
-- * pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:sampleCounts returned by flink:vkGetPhysicalDeviceProperties, or sname:VkImageFormatProperties::pname:sampleCounts returned by fname:vkGetPhysicalDeviceImageFormatProperties with pname:format, pname:type, pname:tiling, pname:usage and pname:flags equal to those in this structure
-- * If pname:usage includes ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, ename:VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT or ename:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT, pname:extent.width must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferWidth
-- * If pname:usage includes ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, ename:VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT or ename:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT, pname:extent.height must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferHeight
-- * If pname:usage includes ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:framebufferColorSampleCounts
-- * If pname:usage includes ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, and pname:format includes a depth aspect, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:framebufferDepthSampleCounts
-- * If pname:usage includes ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, and pname:format includes a stencil aspect, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:framebufferStencilSampleCounts
-- * If pname:usage includes ename:VK_IMAGE_USAGE_SAMPLED_BIT, and pname:format includes a color aspect, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:sampledImageColorSampleCounts
-- * If pname:usage includes ename:VK_IMAGE_USAGE_SAMPLED_BIT, and pname:format includes a depth aspect, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:sampledImageDepthSampleCounts
-- * If pname:usage includes ename:VK_IMAGE_USAGE_SAMPLED_BIT, and pname:format is an integer format, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:sampledImageIntegerSampleCounts
-- * If pname:usage includes ename:VK_IMAGE_USAGE_STORAGE_BIT, pname:samples must: be a bit value that is set in sname:VkPhysicalDeviceLimits::pname:storageImageSampleCounts
-- * If the <<features-features-textureCompressionETC2,ETC2 texture compression>> feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK, ename:VK_FORMAT_EAC_R11_UNORM_BLOCK, ename:VK_FORMAT_EAC_R11_SNORM_BLOCK, ename:VK_FORMAT_EAC_R11G11_UNORM_BLOCK, or ename:VK_FORMAT_EAC_R11G11_SNORM_BLOCK
-- * If the <<features-features-textureCompressionASTC_LDR,ASTC LDR texture compression>> feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_ASTC_4x4_UNORM_BLOCK, ename:VK_FORMAT_ASTC_4x4_SRGB_BLOCK, ename:VK_FORMAT_ASTC_5x4_UNORM_BLOCK, ename:VK_FORMAT_ASTC_5x4_SRGB_BLOCK, ename:VK_FORMAT_ASTC_5x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_5x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_6x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_6x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_6x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_6x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x8_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x8_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x8_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x8_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x10_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x10_SRGB_BLOCK, ename:VK_FORMAT_ASTC_12x10_UNORM_BLOCK, ename:VK_FORMAT_ASTC_12x10_SRGB_BLOCK, ename:VK_FORMAT_ASTC_12x12_UNORM_BLOCK, or ename:VK_FORMAT_ASTC_12x12_SRGB_BLOCK
-- * If the <<features-features-textureCompressionBC,BC texture compression>> feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_BC1_RGB_UNORM_BLOCK, ename:VK_FORMAT_BC1_RGB_SRGB_BLOCK, ename:VK_FORMAT_BC1_RGBA_UNORM_BLOCK, ename:VK_FORMAT_BC1_RGBA_SRGB_BLOCK, ename:VK_FORMAT_BC2_UNORM_BLOCK, ename:VK_FORMAT_BC2_SRGB_BLOCK, ename:VK_FORMAT_BC3_UNORM_BLOCK, ename:VK_FORMAT_BC3_SRGB_BLOCK, ename:VK_FORMAT_BC4_UNORM_BLOCK, ename:VK_FORMAT_BC4_SNORM_BLOCK, ename:VK_FORMAT_BC5_UNORM_BLOCK, ename:VK_FORMAT_BC5_SNORM_BLOCK, ename:VK_FORMAT_BC6H_UFLOAT_BLOCK, ename:VK_FORMAT_BC6H_SFLOAT_BLOCK, ename:VK_FORMAT_BC7_UNORM_BLOCK, or ename:VK_FORMAT_BC7_SRGB_BLOCK
-- * If the <<features-features-shaderStorageImageMultisample,multisampled storage images>> feature is not enabled, and pname:usage contains ename:VK_IMAGE_USAGE_STORAGE_BIT, pname:samples must: be ename:VK_SAMPLE_COUNT_1_BIT
-- * If the <<features-features-sparseBinding,sparse bindings>> feature is not enabled, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_BINDING_BIT
-- * If the <<features-features-sparseResidencyImage2D,sparse residency for 2D images>> feature is not enabled, and pname:imageType is ename:VK_IMAGE_TYPE_2D, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
-- * If the <<features-features-sparseResidencyImage3D,sparse residency for 3D images>> feature is not enabled, and pname:imageType is ename:VK_IMAGE_TYPE_3D, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
-- * If the <<features-features-sparseResidency2Samples,sparse residency for images with 2 samples>> feature is not enabled, pname:imageType is ename:VK_IMAGE_TYPE_2D, and pname:samples is ename:VK_SAMPLE_COUNT_2_BIT, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
-- * If the <<features-features-sparseResidency4Samples,sparse residency for images with 4 samples>> feature is not enabled, pname:imageType is ename:VK_IMAGE_TYPE_2D, and pname:samples is ename:VK_SAMPLE_COUNT_4_BIT, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
-- * If the <<features-features-sparseResidency8Samples,sparse residency for images with 8 samples>> feature is not enabled, pname:imageType is ename:VK_IMAGE_TYPE_2D, and pname:samples is ename:VK_SAMPLE_COUNT_8_BIT, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
-- * If the <<features-features-sparseResidency16Samples,sparse residency for images with 16 samples>> feature is not enabled, pname:imageType is ename:VK_IMAGE_TYPE_2D, and pname:samples is ename:VK_SAMPLE_COUNT_16_BIT, pname:flags mustnot: contain ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
-- * If pname:tiling is ename:VK_IMAGE_TILING_LINEAR, and sname:VkFormatProperties::pname:linearTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_SAMPLED_BIT
-- * If pname:tiling is ename:VK_IMAGE_TILING_LINEAR, and sname:VkFormatProperties::pname:linearTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_STORAGE_BIT
-- * If pname:tiling is ename:VK_IMAGE_TILING_LINEAR, and sname:VkFormatProperties::pname:linearTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
-- * If pname:tiling is ename:VK_IMAGE_TILING_LINEAR, and sname:VkFormatProperties::pname:linearTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
-- * If pname:tiling is ename:VK_IMAGE_TILING_OPTIMAL, and sname:VkFormatProperties::pname:optimalTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_SAMPLED_BIT
-- * If pname:tiling is ename:VK_IMAGE_TILING_OPTIMAL, and sname:VkFormatProperties::pname:optimalTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_STORAGE_BIT
-- * If pname:tiling is ename:VK_IMAGE_TILING_OPTIMAL, and sname:VkFormatProperties::pname:optimalTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
-- * If pname:tiling is ename:VK_IMAGE_TILING_OPTIMAL, and sname:VkFormatProperties::pname:optimalTilingFeatures (as returned by fname:vkGetPhysicalDeviceFormatProperties with the same value of pname:format) does not include ename:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT, pname:usage mustnot: contain ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
-- * If pname:flags contains ename:VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT or ename:VK_IMAGE_CREATE_SPARSE_ALIASED_BIT, it must: also contain ename:VK_IMAGE_CREATE_SPARSE_BINDING_BIT
data VkImageCreateInfo = VkImageCreateInfo
  { vici_sType :: !VkStructureType
  -- ^ `sType` 
  , vici_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vici_flags :: !VkImageCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vici_imageType :: !VkImageType
  -- ^ `imageType` 
  , vici_format :: !VkFormat
  -- ^ `format` 
  , vici_extent :: !VkExtent3D
  -- ^ `extent` 
  , vici_mipLevels :: !Word32
  -- ^ `mipLevels` 
  , vici_arrayLayers :: !Word32
  -- ^ `arrayLayers` 
  , vici_samples :: !VkSampleCountFlagBits
  -- ^ `samples` 
  , vici_tiling :: !VkImageTiling
  -- ^ `tiling` 
  , vici_usage :: !VkImageUsageFlags
  -- ^ `usage` 
  , vici_sharingMode :: !VkSharingMode
  -- ^ `sharingMode` 
  , vici_queueFamilyIndexCount :: !Word32
  -- ^ `queueFamilyIndexCount`  Can be `nullPtr`.
  , vici_pQueueFamilyIndices :: !(Ptr Word32)
  -- ^ `pQueueFamilyIndices`  Const. Length: queueFamilyIndexCount. Noautovalidity.
  , vici_initialLayout :: !VkImageLayout
  -- ^ `initialLayout` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkImageCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 32 + sizeOf (undefined :: CSize) * 9
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    imageType <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    format <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    extent <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4))
    mipLevels <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4))
    arrayLayers <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 4))
    samples <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4))
    tiling <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 5))
    usage <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 6))
    sharingMode <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 6))
    queueFamilyIndexCount <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 7))
    pQueueFamilyIndices <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 7))
    initialLayout <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 8))
    return $ VkImageCreateInfo sType pNext flags imageType format extent mipLevels arrayLayers samples tiling usage sharingMode queueFamilyIndexCount pQueueFamilyIndices initialLayout
  poke ptr (VkImageCreateInfo sType pNext flags imageType format extent mipLevels arrayLayers samples tiling usage sharingMode queueFamilyIndexCount pQueueFamilyIndices initialLayout) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) imageType
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) format
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4)) extent
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4)) mipLevels
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 4)) arrayLayers
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4)) samples
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 5)) tiling
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 6)) usage
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 6)) sharingMode
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 7)) queueFamilyIndexCount
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 7)) pQueueFamilyIndices
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 8)) initialLayout


-- | Man not found. VkSubresourceLayout
-- 
data VkSubresourceLayout = VkSubresourceLayout
  { vsl_offset :: !VkDeviceSize
  -- ^ `offset` 
  , vsl_size :: !VkDeviceSize
  -- ^ `size` 
  , vsl_rowPitch :: !VkDeviceSize
  -- ^ `rowPitch` 
  , vsl_arrayPitch :: !VkDeviceSize
  -- ^ `arrayPitch` 
  , vsl_depthPitch :: !VkDeviceSize
  -- ^ `depthPitch` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkSubresourceLayout where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 40 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    offset <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    size <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    rowPitch <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    arrayPitch <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0))
    depthPitch <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0))
    return $ VkSubresourceLayout offset size rowPitch arrayPitch depthPitch
  poke ptr (VkSubresourceLayout offset size rowPitch arrayPitch depthPitch) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) offset
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) size
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) rowPitch
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0)) arrayPitch
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0)) depthPitch


-- | Man not found. VkImageViewCreateInfo
-- 
-- == Validaty
-- * If pname:image was not created with ename:VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT then pname:viewType mustnot: be ename:VK_IMAGE_VIEW_TYPE_CUBE or ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
-- * If the <<features-features-imageCubeArray,image cubemap arrays>> feature is not enabled, pname:viewType mustnot: be ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
-- * If the <<features-features-textureCompressionETC2,ETC2 texture compression>> feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK, ename:VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK, ename:VK_FORMAT_EAC_R11_UNORM_BLOCK, ename:VK_FORMAT_EAC_R11_SNORM_BLOCK, ename:VK_FORMAT_EAC_R11G11_UNORM_BLOCK, or ename:VK_FORMAT_EAC_R11G11_SNORM_BLOCK
-- * If the <<features-features-textureCompressionASTC_LDR,ASTC LDR texture compression>> feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_ASTC_4x4_UNORM_BLOCK, ename:VK_FORMAT_ASTC_4x4_SRGB_BLOCK, ename:VK_FORMAT_ASTC_5x4_UNORM_BLOCK, ename:VK_FORMAT_ASTC_5x4_SRGB_BLOCK, ename:VK_FORMAT_ASTC_5x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_5x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_6x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_6x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_6x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_6x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_8x8_UNORM_BLOCK, ename:VK_FORMAT_ASTC_8x8_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x5_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x5_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x6_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x6_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x8_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x8_SRGB_BLOCK, ename:VK_FORMAT_ASTC_10x10_UNORM_BLOCK, ename:VK_FORMAT_ASTC_10x10_SRGB_BLOCK, ename:VK_FORMAT_ASTC_12x10_UNORM_BLOCK, ename:VK_FORMAT_ASTC_12x10_SRGB_BLOCK, ename:VK_FORMAT_ASTC_12x12_UNORM_BLOCK, or ename:VK_FORMAT_ASTC_12x12_SRGB_BLOCK
-- * If the <<features-features-textureCompressionBC,BC texture compression>> feature is not enabled, pname:format mustnot: be ename:VK_FORMAT_BC1_RGB_UNORM_BLOCK, ename:VK_FORMAT_BC1_RGB_SRGB_BLOCK, ename:VK_FORMAT_BC1_RGBA_UNORM_BLOCK, ename:VK_FORMAT_BC1_RGBA_SRGB_BLOCK, ename:VK_FORMAT_BC2_UNORM_BLOCK, ename:VK_FORMAT_BC2_SRGB_BLOCK, ename:VK_FORMAT_BC3_UNORM_BLOCK, ename:VK_FORMAT_BC3_SRGB_BLOCK, ename:VK_FORMAT_BC4_UNORM_BLOCK, ename:VK_FORMAT_BC4_SNORM_BLOCK, ename:VK_FORMAT_BC5_UNORM_BLOCK, ename:VK_FORMAT_BC5_SNORM_BLOCK, ename:VK_FORMAT_BC6H_UFLOAT_BLOCK, ename:VK_FORMAT_BC6H_SFLOAT_BLOCK, ename:VK_FORMAT_BC7_UNORM_BLOCK, or ename:VK_FORMAT_BC7_SRGB_BLOCK
-- * If pname:image was created with ename:VK_IMAGE_TILING_LINEAR and pname:usage containing ename:VK_IMAGE_USAGE_SAMPLED_BIT, pname:format must: be supported for sampled images, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:image was created with ename:VK_IMAGE_TILING_LINEAR and pname:usage containing ename:VK_IMAGE_USAGE_STORAGE_BIT, pname:format must: be supported for storage images, as specified by the ename:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:image was created with ename:VK_IMAGE_TILING_LINEAR and pname:usage containing ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, pname:format must: be supported for color attachments, as specified by the ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:image was created with ename:VK_IMAGE_TILING_LINEAR and pname:usage containing ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, pname:format must: be supported for depth/stencil attachments, as specified by the ename:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:image was created with ename:VK_IMAGE_TILING_OPTIMAL and pname:usage containing ename:VK_IMAGE_USAGE_SAMPLED_BIT, pname:format must: be supported for sampled images, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT flag in sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:image was created with ename:VK_IMAGE_TILING_OPTIMAL and pname:usage containing ename:VK_IMAGE_USAGE_STORAGE_BIT, pname:format must: be supported for storage images, as specified by the ename:VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT flag in sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:image was created with ename:VK_IMAGE_TILING_OPTIMAL and pname:usage containing ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, pname:format must: be supported for color attachments, as specified by the ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:image was created with ename:VK_IMAGE_TILING_OPTIMAL and pname:usage containing ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, pname:format must: be supported for depth/stencil attachments, as specified by the ename:VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * pname:subresourceRange must: be a valid image subresource range for pname:image (see <<resources-image-views>>)
-- * If pname:image was created with the ename:VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT flag, pname:format must: be compatible with the pname:format used to create pname:image, as defined in <<features-formats-compatibility-classes,Format Compatibility Classes>>
-- * If pname:image was not created with the ename:VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT flag, pname:format must: be identical to the pname:format used to create pname:image
-- * pname:subResourceRange and pname:viewType must: be compatible with the image, as described in the <<resources-image-views-compatibility,table below>>
data VkImageViewCreateInfo = VkImageViewCreateInfo
  { vivci_sType :: !VkStructureType
  -- ^ `sType` 
  , vivci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vivci_flags :: !VkImageViewCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vivci_image :: !VkImage
  -- ^ `image` 
  , vivci_viewType :: !VkImageViewType
  -- ^ `viewType` 
  , vivci_format :: !VkFormat
  -- ^ `format` 
  , vivci_components :: !VkComponentMapping
  -- ^ `components` 
  , vivci_subresourceRange :: !VkImageSubresourceRange
  -- ^ `subresourceRange` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkImageViewCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 32 + sizeOf (undefined :: CSize) * 8
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    image <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    viewType <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    format <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    components <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    subresourceRange <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 8))
    return $ VkImageViewCreateInfo sType pNext flags image viewType format components subresourceRange
  poke ptr (VkImageViewCreateInfo sType pNext flags image viewType format components subresourceRange) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) image
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) viewType
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) format
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) components
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 8)) subresourceRange


-- | Man not found. VkBufferCopy
-- 
data VkBufferCopy = VkBufferCopy
  { vbc_srcOffset :: !VkDeviceSize
  -- ^ `srcOffset` 
  , vbc_dstOffset :: !VkDeviceSize
  -- ^ `dstOffset` 
  , vbc_size :: !VkDeviceSize
  -- ^ `size` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkBufferCopy where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 24 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    srcOffset <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    dstOffset <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    size <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    return $ VkBufferCopy srcOffset dstOffset size
  poke ptr (VkBufferCopy srcOffset dstOffset size) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) srcOffset
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) dstOffset
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) size


-- | Man not found. VkSparseMemoryBind
-- 
-- == Validaty
-- * If pname:memory is not sname:VK_NULL_HANDLE, pname:memory and pname:memoryOffset must: match the memory requirements of the resource, as described in section <<resources-association>>
-- * If pname:memory is not sname:VK_NULL_HANDLE, pname:memory mustnot: have been created with a memory type that reports ename:VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT bit set
-- * pname:size must: be greater than `0`
-- * pname:resourceOffset must: be less than the size of the resource
-- * pname:size must: be less than or equal to the size of the resource minus pname:resourceOffset
-- * pname:memoryOffset must: be less than the size of pname:memory
-- * pname:size must: be less than or equal to the size of pname:memory minus pname:memoryOffset
data VkSparseMemoryBind = VkSparseMemoryBind
  { vsmb_resourceOffset :: !VkDeviceSize
  -- ^ `resourceOffset` 
  , vsmb_size :: !VkDeviceSize
  -- ^ `size` 
  , vsmb_memory :: !VkDeviceMemory
  -- ^ `memory`  Can be `nullPtr`.
  , vsmb_memoryOffset :: !VkDeviceSize
  -- ^ `memoryOffset` 
  , vsmb_flags :: !VkSparseMemoryBindFlags
  -- ^ `flags`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSparseMemoryBind where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 36 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    resourceOffset <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    size <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    memory <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    memoryOffset <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0))
    flags <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0))
    return $ VkSparseMemoryBind resourceOffset size memory memoryOffset flags
  poke ptr (VkSparseMemoryBind resourceOffset size memory memoryOffset flags) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) resourceOffset
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) size
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) memory
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0)) memoryOffset
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0)) flags


-- | Man not found. VkSparseImageMemoryBind
-- 
-- == Validaty
-- * If the <<features-features-sparseResidencyAliased,sparse aliased residency>> feature is not enabled, and if any other resources are bound to ranges of pname:memory, the range of pname:memory being bound mustnot: overlap with those bound ranges
-- * pname:memory and pname:memoryOffset must: match the memory requirements of the calling command's pname:image, as described in section <<resources-association>>
-- * pname:subresource must: be a valid image subresource for pname:image (see <<resources-image-views>>)
-- * pname:offset.x must: be a multiple of the sparse image block width (sname:VkSparseImageFormatProperties::pname:imageGranularity.width) of the image
-- * pname:extent.width must: either be a multiple of the sparse image block width of the image, or else pname:extent.width + pname:offset.x must: equal the width of the image subresource
-- * pname:offset.y must: be a multiple of the sparse image block height (sname:VkSparseImageFormatProperties::pname:imageGranularity.height) of the image
-- * pname:extent.height must: either be a multiple of the sparse image block height of the image, or else pname:extent.height + pname:offset.y must: equal the height of the image subresource
-- * pname:offset.z must: be a multiple of the sparse image block depth (sname:VkSparseImageFormatProperties::pname:imageGranularity.depth) of the image
-- * pname:extent.depth must: either be a multiple of the sparse image block depth of the image, or else pname:extent.depth + pname:offset.z must: equal the depth of the image subresource
data VkSparseImageMemoryBind = VkSparseImageMemoryBind
  { vsimb_subresource :: !VkImageSubresource
  -- ^ `subresource` 
  , vsimb_offset :: !VkOffset3D
  -- ^ `offset` 
  , vsimb_extent :: !VkExtent3D
  -- ^ `extent` 
  , vsimb_memory :: !VkDeviceMemory
  -- ^ `memory`  Can be `nullPtr`.
  , vsimb_memoryOffset :: !VkDeviceSize
  -- ^ `memoryOffset` 
  , vsimb_flags :: !VkSparseMemoryBindFlags
  -- ^ `flags`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSparseImageMemoryBind where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 56 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    subresource <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    offset <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    extent <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0))
    memory <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0))
    memoryOffset <- peek (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0))
    flags <- peek (plusPtr ptr (52 + sizeOf (undefined :: CSize) * 0))
    return $ VkSparseImageMemoryBind subresource offset extent memory memoryOffset flags
  poke ptr (VkSparseImageMemoryBind subresource offset extent memory memoryOffset flags) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) subresource
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) offset
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0)) extent
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0)) memory
    poke (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0)) memoryOffset
    poke (plusPtr ptr (52 + sizeOf (undefined :: CSize) * 0)) flags


-- | Man not found. VkSparseBufferMemoryBindInfo
-- 
data VkSparseBufferMemoryBindInfo = VkSparseBufferMemoryBindInfo
  { vsbmbi_buffer :: !VkBuffer
  -- ^ `buffer` 
  , vsbmbi_bindCount :: !Word32
  -- ^ `bindCount` 
  , vsbmbi_pBinds :: !(Ptr VkSparseMemoryBind)
  -- ^ `pBinds`  Const. Length: bindCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSparseBufferMemoryBindInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    buffer <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    bindCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    pBinds <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    return $ VkSparseBufferMemoryBindInfo buffer bindCount pBinds
  poke ptr (VkSparseBufferMemoryBindInfo buffer bindCount pBinds) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) buffer
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) bindCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) pBinds


-- | Man not found. VkSparseImageOpaqueMemoryBindInfo
-- 
-- == Validaty
-- * For any given element of pname:pBinds, if the pname:flags member of that element contains ename:VK_SPARSE_MEMORY_BIND_METADATA_BIT, the binding range defined must: be within the mip tail region of the metadata aspect of pname:image
data VkSparseImageOpaqueMemoryBindInfo = VkSparseImageOpaqueMemoryBindInfo
  { vsiombi_image :: !VkImage
  -- ^ `image` 
  , vsiombi_bindCount :: !Word32
  -- ^ `bindCount` 
  , vsiombi_pBinds :: !(Ptr VkSparseMemoryBind)
  -- ^ `pBinds`  Const. Length: bindCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSparseImageOpaqueMemoryBindInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    image <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    bindCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    pBinds <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    return $ VkSparseImageOpaqueMemoryBindInfo image bindCount pBinds
  poke ptr (VkSparseImageOpaqueMemoryBindInfo image bindCount pBinds) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) image
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) bindCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) pBinds


-- | Man not found. VkSparseImageMemoryBindInfo
-- 
data VkSparseImageMemoryBindInfo = VkSparseImageMemoryBindInfo
  { vsimbi_image :: !VkImage
  -- ^ `image` 
  , vsimbi_bindCount :: !Word32
  -- ^ `bindCount` 
  , vsimbi_pBinds :: !(Ptr VkSparseImageMemoryBind)
  -- ^ `pBinds`  Const. Length: bindCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSparseImageMemoryBindInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    image <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    bindCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    pBinds <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    return $ VkSparseImageMemoryBindInfo image bindCount pBinds
  poke ptr (VkSparseImageMemoryBindInfo image bindCount pBinds) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) image
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) bindCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) pBinds


-- | Man not found. VkBindSparseInfo
-- 
data VkBindSparseInfo = VkBindSparseInfo
  { vbsi_sType :: !VkStructureType
  -- ^ `sType` 
  , vbsi_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vbsi_waitSemaphoreCount :: !Word32
  -- ^ `waitSemaphoreCount`  Can be `nullPtr`.
  , vbsi_pWaitSemaphores :: !(Ptr VkSemaphore)
  -- ^ `pWaitSemaphores`  Const. Length: waitSemaphoreCount.
  , vbsi_bufferBindCount :: !Word32
  -- ^ `bufferBindCount`  Can be `nullPtr`.
  , vbsi_pBufferBinds :: !(Ptr VkSparseBufferMemoryBindInfo)
  -- ^ `pBufferBinds`  Const. Length: bufferBindCount.
  , vbsi_imageOpaqueBindCount :: !Word32
  -- ^ `imageOpaqueBindCount`  Can be `nullPtr`.
  , vbsi_pImageOpaqueBinds :: !(Ptr VkSparseImageOpaqueMemoryBindInfo)
  -- ^ `pImageOpaqueBinds`  Const. Length: imageOpaqueBindCount.
  , vbsi_imageBindCount :: !Word32
  -- ^ `imageBindCount`  Can be `nullPtr`.
  , vbsi_pImageBinds :: !(Ptr VkSparseImageMemoryBindInfo)
  -- ^ `pImageBinds`  Const. Length: imageBindCount.
  , vbsi_signalSemaphoreCount :: !Word32
  -- ^ `signalSemaphoreCount`  Can be `nullPtr`.
  , vbsi_pSignalSemaphores :: !(Ptr VkSemaphore)
  -- ^ `pSignalSemaphores`  Const. Length: signalSemaphoreCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkBindSparseInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 7
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    waitSemaphoreCount <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    pWaitSemaphores <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    bufferBindCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    pBufferBinds <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    imageOpaqueBindCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4))
    pImageOpaqueBinds <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    imageBindCount <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 5))
    pImageBinds <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 5))
    signalSemaphoreCount <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 6))
    pSignalSemaphores <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 6))
    return $ VkBindSparseInfo sType pNext waitSemaphoreCount pWaitSemaphores bufferBindCount pBufferBinds imageOpaqueBindCount pImageOpaqueBinds imageBindCount pImageBinds signalSemaphoreCount pSignalSemaphores
  poke ptr (VkBindSparseInfo sType pNext waitSemaphoreCount pWaitSemaphores bufferBindCount pBufferBinds imageOpaqueBindCount pImageOpaqueBinds imageBindCount pImageBinds signalSemaphoreCount pSignalSemaphores) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) waitSemaphoreCount
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) pWaitSemaphores
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) bufferBindCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) pBufferBinds
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4)) imageOpaqueBindCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) pImageOpaqueBinds
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 5)) imageBindCount
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 5)) pImageBinds
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 6)) signalSemaphoreCount
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 6)) pSignalSemaphores


-- | Man not found. VkImageCopy
-- 
-- == Validaty
-- * The pname:aspectMask member of pname:srcSubresource and pname:dstSubresource must: match
-- * The pname:layerCount member of pname:srcSubresource and pname:dstSubresource must: match
-- * If either of the calling command's pname:srcImage or pname:dstImage parameters are of elink:VkImageType ename:VK_IMAGE_TYPE_3D, the pname:baseArrayLayer and pname:layerCount members of both pname:srcSubresource and pname:dstSubresource must: be `0` and `1`, respectively
-- * The pname:aspectMask member of pname:srcSubresource must: specify aspects present in the calling command's pname:srcImage
-- * The pname:aspectMask member of pname:dstSubresource must: specify aspects present in the calling command's pname:dstImage
-- * pname:srcOffset.x and (pname:extent.width + pname:srcOffset.x) must: both be greater than or equal to `0` and less than or equal to the source image subresource width
-- * pname:srcOffset.y and (pname:extent.height + pname:srcOffset.y) must: both be greater than or equal to `0` and less than or equal to the source image subresource height
-- * pname:srcOffset.z and (pname:extent.depth + pname:srcOffset.z) must: both be greater than or equal to `0` and less than or equal to the source image subresource depth
-- * pname:dstOffset.x and (pname:extent.width + pname:dstOffset.x) must: both be greater than or equal to `0` and less than or equal to the destination image subresource width
-- * pname:dstOffset.y and (pname:extent.height + pname:dstOffset.y) must: both be greater than or equal to `0` and less than or equal to the destination image subresource height
-- * pname:dstOffset.z and (pname:extent.depth + pname:dstOffset.z) must: both be greater than or equal to `0` and less than or equal to the destination image subresource depth
-- * If the calling command's pname:srcImage is a compressed format image:
--                         ** all members of pname:srcOffset must: be a multiple of the corresponding dimensions of the compressed texel block
--                         ** pname:extent.width must: be a multiple of the compressed texel block width or (pname:extent.width + pname:srcOffset.x) must: equal the source image subresource width
--                         ** pname:extent.height must: be a multiple of the compressed texel block height or (pname:extent.height + pname:srcOffset.y) must: equal the source image subresource height
--                         ** pname:extent.depth must: be a multiple of the compressed texel block depth or (pname:extent.depth + pname:srcOffset.z) must: equal the source image subresource depth
-- * If the calling command's pname:dstImage is a compressed format image:
--                         ** all members of pname:dstOffset must: be a multiple of the corresponding dimensions of the compressed texel block
--                         ** pname:extent.width must: be a multiple of the compressed texel block width or (pname:extent.width + pname:dstOffset.x) must: equal the destination image subresource width
--                         ** pname:extent.height must: be a multiple of the compressed texel block height or (pname:extent.height + pname:dstOffset.y) must: equal the destination image subresource height
--                         ** pname:extent.depth must: be a multiple of the compressed texel block depth or (pname:extent.depth + pname:dstOffset.z) must: equal the destination image subresource depth
-- * pname:srcOffset, pname:dstOffset, and pname:extent must: respect the image transfer granularity requirements of the queue family that it will be submitted against, as described in <<devsandqueues-physical-device-enumeration,Physical Device Enumeration>>
data VkImageCopy = VkImageCopy
  { vic_srcSubresource :: !VkImageSubresourceLayers
  -- ^ `srcSubresource` 
  , vic_srcOffset :: !VkOffset3D
  -- ^ `srcOffset` 
  , vic_dstSubresource :: !VkImageSubresourceLayers
  -- ^ `dstSubresource` 
  , vic_dstOffset :: !VkOffset3D
  -- ^ `dstOffset` 
  , vic_extent :: !VkExtent3D
  -- ^ `extent` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkImageCopy where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 68 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    srcSubresource <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    srcOffset <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    dstSubresource <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0))
    dstOffset <- peek (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0))
    extent <- peek (plusPtr ptr (56 + sizeOf (undefined :: CSize) * 0))
    return $ VkImageCopy srcSubresource srcOffset dstSubresource dstOffset extent
  poke ptr (VkImageCopy srcSubresource srcOffset dstSubresource dstOffset extent) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) srcSubresource
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) srcOffset
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0)) dstSubresource
    poke (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0)) dstOffset
    poke (plusPtr ptr (56 + sizeOf (undefined :: CSize) * 0)) extent


-- | Man not found. VkImageBlit
-- 
-- == Validaty
-- * The pname:aspectMask member of pname:srcSubresource and pname:dstSubresource must: match
-- * The pname:layerCount member of pname:srcSubresource and pname:dstSubresource must: match
-- * If either of the calling command's pname:srcImage or pname:dstImage parameters are of elink:VkImageType ename:VK_IMAGE_TYPE_3D, the pname:baseArrayLayer and pname:layerCount members of both pname:srcSubresource and pname:dstSubresource must: be `0` and `1`, respectively
-- * The pname:aspectMask member of pname:srcSubresource must: specify aspects present in the calling command's pname:srcImage
-- * The pname:aspectMask member of pname:dstSubresource must: specify aspects present in the calling command's pname:dstImage
-- * pname:srcOffset[0].x and pname:srcOffset[1].x must: both be greater than or equal to `0` and less than or equal to the source image subresource width
-- * pname:srcOffset[0].y and pname:srcOffset[1].y must: both be greater than or equal to `0` and less than or equal to the source image subresource height
-- * pname:srcOffset[0].z and pname:srcOffset[1].z must: both be greater than or equal to `0` and less than or equal to the source image subresource depth
-- * pname:dstOffset[0].x and pname:dstOffset[1].x must: both be greater than or equal to `0` and less than or equal to the destination image subresource width
-- * pname:dstOffset[0].y and pname:dstOffset[1].y must: both be greater than or equal to `0` and less than or equal to the destination image subresource height
-- * pname:dstOffset[0].z and pname:dstOffset[1].z must: both be greater than or equal to `0` and less than or equal to the destination image subresource depth
data VkImageBlit = VkImageBlit
  { vib_srcSubresource :: !VkImageSubresourceLayers
  -- ^ `srcSubresource` 
  , vib_srcOffsets :: !(V2 VkOffset3D)
  -- ^ `srcOffsets`  Max: [2].
  , vib_dstSubresource :: !VkImageSubresourceLayers
  -- ^ `dstSubresource` 
  , vib_dstOffsets :: !(V2 VkOffset3D)
  -- ^ `dstOffsets`  Max: [2].
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkImageBlit where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 80 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    srcSubresource <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    srcOffsets <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    dstSubresource <- peek (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0))
    dstOffsets <- peek (plusPtr ptr (56 + sizeOf (undefined :: CSize) * 0))
    return $ VkImageBlit srcSubresource srcOffsets dstSubresource dstOffsets
  poke ptr (VkImageBlit srcSubresource srcOffsets dstSubresource dstOffsets) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) srcSubresource
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) srcOffsets
    poke (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0)) dstSubresource
    poke (plusPtr ptr (56 + sizeOf (undefined :: CSize) * 0)) dstOffsets


-- | Man not found. VkBufferImageCopy
-- 
-- == Validaty
-- * pname:bufferOffset must: be a multiple of the calling command's sname:VkImage parameter's texel size
-- * pname:bufferOffset must: be a multiple of `4`
-- * pname:bufferRowLength must: be `0`, or greater than or equal to the pname:width member of pname:imageExtent
-- * pname:bufferImageHeight must: be `0`, or greater than or equal to the pname:height member of pname:imageExtent
-- * pname:imageOffset.x and (pname:imageExtent.width + pname:imageOffset.x) must: both be greater than or equal to `0` and less than or equal to the image subresource width
-- * pname:imageOffset.y and (imageExtent.height + pname:imageOffset.y) must: both be greater than or equal to `0` and less than or equal to the image subresource height
-- * pname:imageOffset.z and (imageExtent.depth + pname:imageOffset.z) must: both be greater than or equal to `0` and less than or equal to the image subresource depth
-- * If the calling command's sname:VkImage parameter is a compressed format image:
--                         ** pname:bufferRowLength must: be a multiple of the compressed texel block width
--                         ** pname:bufferImageHeight must: be a multiple of the compressed texel block height
--                         ** all members of pname:imageOffset must: be a multiple of the corresponding dimensions of the compressed texel block
--                         ** pname:bufferOffset must: be a multiple of the compressed texel block size in bytes
--                         ** pname:imageExtent.width must: be a multiple of the compressed texel block width or (pname:imageExtent.width + pname:imageOffset.x) must: equal the image subresource width
--                         ** pname:imageExtent.height must: be a multiple of the compressed texel block height or (pname:imageExtent.height + pname:imageOffset.y) must: equal the image subresource height
--                         ** pname:imageExtent.depth must: be a multiple of the compressed texel block depth or (pname:imageExtent.depth + pname:imageOffset.z) must: equal the image subresource depth
-- * pname:bufferOffset, pname:bufferRowLength, pname:bufferImageHeight and all members of pname:imageOffset and pname:imageExtent must: respect the image transfer granularity requirements of the queue family that it will be submitted against, as described in <<devsandqueues-physical-device-enumeration,Physical Device Enumeration>>
-- * The pname:aspectMask member of pname:imageSubresource must: specify aspects present in the calling command's sname:VkImage parameter
-- * The pname:aspectMask member of pname:imageSubresource must: only have a single bit set
-- * If the calling command's sname:VkImage parameter is of elink:VkImageType ename:VK_IMAGE_TYPE_3D, the pname:baseArrayLayer and pname:layerCount members of pname:imageSubresource must: be `0` and `1`, respectively
data VkBufferImageCopy = VkBufferImageCopy
  { vbic_bufferOffset :: !VkDeviceSize
  -- ^ `bufferOffset` 
  , vbic_bufferRowLength :: !Word32
  -- ^ `bufferRowLength` 
  , vbic_bufferImageHeight :: !Word32
  -- ^ `bufferImageHeight` 
  , vbic_imageSubresource :: !VkImageSubresourceLayers
  -- ^ `imageSubresource` 
  , vbic_imageOffset :: !VkOffset3D
  -- ^ `imageOffset` 
  , vbic_imageExtent :: !VkExtent3D
  -- ^ `imageExtent` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkBufferImageCopy where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 56 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    bufferOffset <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    bufferRowLength <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    bufferImageHeight <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    imageSubresource <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    imageOffset <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0))
    imageExtent <- peek (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0))
    return $ VkBufferImageCopy bufferOffset bufferRowLength bufferImageHeight imageSubresource imageOffset imageExtent
  poke ptr (VkBufferImageCopy bufferOffset bufferRowLength bufferImageHeight imageSubresource imageOffset imageExtent) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) bufferOffset
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) bufferRowLength
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) bufferImageHeight
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) imageSubresource
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0)) imageOffset
    poke (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0)) imageExtent


-- | Man not found. VkImageResolve
-- 
-- == Validaty
-- * The pname:aspectMask member of pname:srcSubresource and pname:dstSubresource must: only contain ename:VK_IMAGE_ASPECT_COLOR_BIT
-- * The pname:layerCount member of pname:srcSubresource and pname:dstSubresource must: match
-- * If either of the calling command's pname:srcImage or pname:dstImage parameters are of elink:VkImageType ename:VK_IMAGE_TYPE_3D, the pname:baseArrayLayer and pname:layerCount members of both pname:srcSubresource and pname:dstSubresource must: be `0` and `1`, respectively
data VkImageResolve = VkImageResolve
  { vir_srcSubresource :: !VkImageSubresourceLayers
  -- ^ `srcSubresource` 
  , vir_srcOffset :: !VkOffset3D
  -- ^ `srcOffset` 
  , vir_dstSubresource :: !VkImageSubresourceLayers
  -- ^ `dstSubresource` 
  , vir_dstOffset :: !VkOffset3D
  -- ^ `dstOffset` 
  , vir_extent :: !VkExtent3D
  -- ^ `extent` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkImageResolve where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 68 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    srcSubresource <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    srcOffset <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    dstSubresource <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0))
    dstOffset <- peek (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0))
    extent <- peek (plusPtr ptr (56 + sizeOf (undefined :: CSize) * 0))
    return $ VkImageResolve srcSubresource srcOffset dstSubresource dstOffset extent
  poke ptr (VkImageResolve srcSubresource srcOffset dstSubresource dstOffset extent) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) srcSubresource
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) srcOffset
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0)) dstSubresource
    poke (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0)) dstOffset
    poke (plusPtr ptr (56 + sizeOf (undefined :: CSize) * 0)) extent


-- | Man not found. VkShaderModuleCreateInfo
-- 
-- == Validaty
-- * pname:codeSize must: be greater than 0
-- * pname:codeSize must: be a multiple of 4
-- * pname:pCode must: point to valid SPIR-V code, formatted and packed as described by https://www.khronos.org/registry/spir-v/specs/1.0/SPIRV.html[the SPIR-V Specification v1.0]
-- * pname:pCode must: adhere to the validation rules described by the <<spirvenv-module-validation, Validation Rules within a Module>> section of the <<spirvenv-capabilities,SPIR-V Environment>> appendix
-- * pname:pCode must: declare the code:Shader capability
-- * pname:pCode mustnot: declare any capability that is not supported by the API, as described by the <<spirvenv-module-validation, Capabilities>> section of the <<spirvenv-capabilities,SPIR-V Environment>> appendix
-- * If pname:pCode declares any of the capabilities that are listed as not required by the implementation, the relevant feature must: be enabled, as listed in the <<spirvenv-capabilities-table,SPIR-V Environment>> appendix
data VkShaderModuleCreateInfo = VkShaderModuleCreateInfo
  { vsmci_sType :: !VkStructureType
  -- ^ `sType` 
  , vsmci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vsmci_flags :: !VkShaderModuleCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vsmci_codeSize :: !CSize
  -- ^ `codeSize` 
  , vsmci_pCode :: !(Ptr Word32)
  -- ^ `pCode`  Const. Length: latexmath:[$codeSize \over 4$].
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkShaderModuleCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    codeSize <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pCode <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    return $ VkShaderModuleCreateInfo sType pNext flags codeSize pCode
  poke ptr (VkShaderModuleCreateInfo sType pNext flags codeSize pCode) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) codeSize
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) pCode


-- | Man not found. VkDescriptorSetLayoutBinding
-- 
-- == Validaty
-- * If pname:descriptorType is ename:VK_DESCRIPTOR_TYPE_SAMPLER or ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, and pname:descriptorCount is not `0` and pname:pImmutableSamplers is not `NULL`, pname:pImmutableSamplers must: be a pointer to an array of pname:descriptorCount valid sname:VkSampler handles
-- * If pname:descriptorCount is not `0`, pname:stageFlags must: be a valid combination of elink:VkShaderStageFlagBits values
data VkDescriptorSetLayoutBinding = VkDescriptorSetLayoutBinding
  { vdslb_binding :: !Word32
  -- ^ `binding` 
  , vdslb_descriptorType :: !VkDescriptorType
  -- ^ `descriptorType` 
  , vdslb_descriptorCount :: !Word32
  -- ^ `descriptorCount`  Can be `nullPtr`.
  , vdslb_stageFlags :: !VkShaderStageFlags
  -- ^ `stageFlags`  Noautovalidity.
  , vdslb_pImmutableSamplers :: !(Ptr VkSampler)
  -- ^ `pImmutableSamplers`  Can be `nullPtr`. Const. Length: descriptorCount. Noautovalidity.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDescriptorSetLayoutBinding where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    binding <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    descriptorType <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    descriptorCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1))
    stageFlags <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 1))
    pImmutableSamplers <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 1))
    return $ VkDescriptorSetLayoutBinding binding descriptorType descriptorCount stageFlags pImmutableSamplers
  poke ptr (VkDescriptorSetLayoutBinding binding descriptorType descriptorCount stageFlags pImmutableSamplers) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) binding
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) descriptorType
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1)) descriptorCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 1)) stageFlags
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 1)) pImmutableSamplers


-- | Man not found. VkDescriptorSetLayoutCreateInfo
-- 
data VkDescriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo
  { vdslci_sType :: !VkStructureType
  -- ^ `sType` 
  , vdslci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdslci_flags :: !VkDescriptorSetLayoutCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vdslci_bindingCount :: !Word32
  -- ^ `bindingCount`  Can be `nullPtr`.
  , vdslci_pBindings :: !(Ptr VkDescriptorSetLayoutBinding)
  -- ^ `pBindings`  Const. Length: bindingCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDescriptorSetLayoutCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    bindingCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pBindings <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    return $ VkDescriptorSetLayoutCreateInfo sType pNext flags bindingCount pBindings
  poke ptr (VkDescriptorSetLayoutCreateInfo sType pNext flags bindingCount pBindings) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) bindingCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) pBindings


-- | Man not found. VkDescriptorPoolSize
-- 
-- == Validaty
-- * pname:descriptorCount must: be greater than `0`
data VkDescriptorPoolSize = VkDescriptorPoolSize
  { vdps_imageType :: !VkDescriptorType
  -- ^ `imageType` 
  , vdps_descriptorCount :: !Word32
  -- ^ `descriptorCount` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDescriptorPoolSize where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    imageType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    descriptorCount <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    return $ VkDescriptorPoolSize imageType descriptorCount
  poke ptr (VkDescriptorPoolSize imageType descriptorCount) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) imageType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) descriptorCount


-- | Man not found. VkDescriptorPoolCreateInfo
-- 
-- == Validaty
-- * pname:maxSets must: be greater than `0`
data VkDescriptorPoolCreateInfo = VkDescriptorPoolCreateInfo
  { vdpci_sType :: !VkStructureType
  -- ^ `sType` 
  , vdpci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdpci_flags :: !VkDescriptorPoolCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vdpci_maxSets :: !Word32
  -- ^ `maxSets` 
  , vdpci_poolSizeCount :: !Word32
  -- ^ `poolSizeCount` 
  , vdpci_pPoolSizes :: !(Ptr VkDescriptorPoolSize)
  -- ^ `pPoolSizes`  Const. Length: poolSizeCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDescriptorPoolCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    maxSets <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    poolSizeCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    pPoolSizes <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    return $ VkDescriptorPoolCreateInfo sType pNext flags maxSets poolSizeCount pPoolSizes
  poke ptr (VkDescriptorPoolCreateInfo sType pNext flags maxSets poolSizeCount pPoolSizes) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) maxSets
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) poolSizeCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) pPoolSizes


-- | Structure specifying the allocation parameters for descriptor sets..
-- Just ["Structure type. Must be ename:VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO.","Pointer to next structure in the structure chain when applicable.","The pool from which to allocate the descriptor sets.","The number of descriptor sets to allocate.","An array of pname:descriptorSetCount handles to descriptor set layouts objects describing the descriptor sets."]
-- Nothing
-- Just ["flink:vkAllocateDescriptorSets"]
-- 
-- This structure is used to specify the parameters of descriptor set objects allocated using
-- flink:vkAllocateDescriptorSets.
--
-- include::../validity/structs/VkDescriptorSetAllocateInfo.txt[]
-- 
-- == Validaty
-- * pname:descriptorSetCount mustnot: be greater than the number of sets that are currently available for allocation in pname:descriptorPool
-- * pname:descriptorPool must: have enough free descriptor capacity remaining to allocate the descriptor sets of the specified layouts
data VkDescriptorSetAllocateInfo = VkDescriptorSetAllocateInfo
  { vdsai_sType :: !VkStructureType
  -- ^ `sType` 
  , vdsai_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdsai_descriptorPool :: !VkDescriptorPool
  -- ^ `descriptorPool` 
  , vdsai_descriptorSetCount :: !Word32
  -- ^ `descriptorSetCount` 
  , vdsai_pSetLayouts :: !(Ptr VkDescriptorSetLayout)
  -- ^ `pSetLayouts`  Const. Length: descriptorSetCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDescriptorSetAllocateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    descriptorPool <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    descriptorSetCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    pSetLayouts <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    return $ VkDescriptorSetAllocateInfo sType pNext descriptorPool descriptorSetCount pSetLayouts
  poke ptr (VkDescriptorSetAllocateInfo sType pNext descriptorPool descriptorSetCount pSetLayouts) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) descriptorPool
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) descriptorSetCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) pSetLayouts


-- | Man not found. VkSpecializationMapEntry
-- 
data VkSpecializationMapEntry = VkSpecializationMapEntry
  { vsme_constantID :: !Word32
  -- ^ `constantID` 
  , vsme_offset :: !Word32
  -- ^ `offset` 
  , vsme_size :: !CSize
  -- ^ `size` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSpecializationMapEntry where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    constantID <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    offset <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    size <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkSpecializationMapEntry constantID offset size
  poke ptr (VkSpecializationMapEntry constantID offset size) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) constantID
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) offset
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) size


-- | Man not found. VkSpecializationInfo
-- 
-- == Validaty
-- * The pname:offset member of any given element of pname:pMapEntries must: be less than pname:dataSize
-- * For any given element of pname:pMapEntries, pname:size must: be less than or equal to pname:dataSize minus pname:offset
data VkSpecializationInfo = VkSpecializationInfo
  { vsi_mapEntryCount :: !Word32
  -- ^ `mapEntryCount`  Can be `nullPtr`.
  , vsi_pMapEntries :: !(Ptr VkSpecializationMapEntry)
  -- ^ `pMapEntries`  Const. Length: mapEntryCount.
  , vsi_dataSize :: !CSize
  -- ^ `dataSize`  Can be `nullPtr`.
  , vsi_pData :: !(Ptr ())
  -- ^ `pData`  Const. Length: dataSize.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSpecializationInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    mapEntryCount <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pMapEntries <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    dataSize <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1))
    pData <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    return $ VkSpecializationInfo mapEntryCount pMapEntries dataSize pData
  poke ptr (VkSpecializationInfo mapEntryCount pMapEntries dataSize pData) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) mapEntryCount
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) pMapEntries
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1)) dataSize
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) pData


-- | Man not found. VkPipelineShaderStageCreateInfo
-- 
-- == Validaty
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:stage mustnot: be ename:VK_SHADER_STAGE_GEOMETRY_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:stage mustnot: be ename:VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT or ename:VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
-- * pname:stage mustnot: be ename:VK_SHADER_STAGE_ALL_GRAPHICS, or ename:VK_SHADER_STAGE_ALL
-- * pname:pName must: be the name of an code:OpEntryPoint in pname:module with an execution model that matches pname:stage
-- * If the identified entry point includes any variable in its interface that is declared with the code:ClipDistance code:BuiltIn decoration, that variable mustnot: have an array size greater than sname:VkPhysicalDeviceLimits::pname:maxClipDistances
-- * If the identified entry point includes any variable in its interface that is declared with the code:CullDistance code:BuiltIn decoration, that variable mustnot: have an array size greater than sname:VkPhysicalDeviceLimits::pname:maxCullDistances
-- * If the identified entry point includes any variables in its interface that are declared with the code:ClipDistance or code:CullDistance code:BuiltIn decoration, those variables mustnot: have array sizes which sum to more than sname:VkPhysicalDeviceLimits::pname:maxCombinedClipAndCullDistances
-- * If the identified entry point includes any variable in its interface that is declared with the code:SampleMask code:BuiltIn decoration, that variable mustnot: have an array size greater than sname:VkPhysicalDeviceLimits::pname:maxSampleMaskWords
-- * If pname:stage is ename:VK_SHADER_STAGE_VERTEX_BIT, the identified entry point mustnot: include any input variable in its interface that is decorated with code:CullDistance
-- * If pname:stage is ename:VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT or ename:VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT, and the identified entry point has an code:OpExecutionMode instruction that specifies a patch size with code:OutputVertices, the patch size must: be greater than `0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxTessellationPatchSize
-- * If pname:stage is ename:VK_SHADER_STAGE_GEOMETRY_BIT, the identified entry point must: have an code:OpExecutionMode instruction that specifies a maximum output vertex count that is greater than `0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxGeometryOutputVertices
-- * If pname:stage is ename:VK_SHADER_STAGE_GEOMETRY_BIT, the identified entry point must: have an code:OpExecutionMode instruction that specifies an invocation count that is greater than `0` and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxGeometryShaderInvocations
-- * If pname:stage is ename:VK_SHADER_STAGE_GEOMETRY_BIT, and the identified entry point writes to code:Layer for any primitive, it must: write the same value to code:Layer for all vertices of a given primitive
-- * If pname:stage is ename:VK_SHADER_STAGE_GEOMETRY_BIT, and the identified entry point writes to code:ViewportIndex for any primitive, it must: write the same value to code:ViewportIndex for all vertices of a given primitive
-- * If pname:stage is ename:VK_SHADER_STAGE_FRAGMENT_BIT, the identified entry point mustnot: include any output variables in its interface decorated with code:CullDistance
-- * If pname:stage is ename:VK_SHADER_STAGE_FRAGMENT_BIT, and the identified entry point writes to code:FragDepth in any execution path, it must: write to code:FragDepth in all execution paths
data VkPipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo
  { vpssci_sType :: !VkStructureType
  -- ^ `sType` 
  , vpssci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpssci_flags :: !VkPipelineShaderStageCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vpssci_stage :: !VkShaderStageFlagBits
  -- ^ `stage` 
  , vpssci_shaderModule :: !VkShaderModule
  -- ^ `shaderModule` 
  , vpssci_pName :: !(Ptr CChar)
  -- ^ `pName`  Const. Length: null-terminated.
  , vpssci_pSpecializationInfo :: !(Ptr VkSpecializationInfo)
  -- ^ `pSpecializationInfo`  Can be `nullPtr`. Const.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineShaderStageCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 5
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    stage <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    shaderModule <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    pName <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    pSpecializationInfo <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    return $ VkPipelineShaderStageCreateInfo sType pNext flags stage shaderModule pName pSpecializationInfo
  poke ptr (VkPipelineShaderStageCreateInfo sType pNext flags stage shaderModule pName pSpecializationInfo) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) stage
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) shaderModule
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) pName
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) pSpecializationInfo


-- | Man not found. VkComputePipelineCreateInfo
-- 
-- == Validaty
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineIndex is not `-1`, pname:basePipelineHandle must: be sname:VK_NULL_HANDLE
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineIndex is not `-1`, it must: be a valid index into the calling command's pname:pCreateInfos parameter
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, pname:basePipelineIndex must: be `-1`
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, pname:basePipelineHandle must: be a valid sname:VkPipeline handle
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, it must: be a valid handle to a compute sname:VkPipeline
-- * The pname:stage member of pname:stage must: be ename:VK_SHADER_STAGE_COMPUTE_BIT
-- * The shader code for the entry point identified by pname:stage and the rest of the state identified by this structure must: adhere to the pipeline linking rules described in the <<interfaces,Shader Interfaces>> chapter
-- * pname:layout must: be <<descriptorsets-pipelinelayout-consistency,consistent>> with all shaders specified in pname:pStages
data VkComputePipelineCreateInfo = VkComputePipelineCreateInfo
  { vcpci_sType :: !VkStructureType
  -- ^ `sType` 
  , vcpci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vcpci_flags :: !VkPipelineCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vcpci_stage :: !VkPipelineShaderStageCreateInfo
  -- ^ `stage` 
  , vcpci_layout :: !VkPipelineLayout
  -- ^ `layout` 
  , vcpci_basePipelineHandle :: !VkPipeline
  -- ^ `basePipelineHandle`  Can be `nullPtr`. Noautovalidity.
  , vcpci_basePipelineIndex :: !Int32
  -- ^ `basePipelineIndex` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkComputePipelineCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 36 + sizeOf (undefined :: CSize) * 7
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    stage <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    layout <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 7))
    basePipelineHandle <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 7))
    basePipelineIndex <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 7))
    return $ VkComputePipelineCreateInfo sType pNext flags stage layout basePipelineHandle basePipelineIndex
  poke ptr (VkComputePipelineCreateInfo sType pNext flags stage layout basePipelineHandle basePipelineIndex) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) stage
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 7)) layout
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 7)) basePipelineHandle
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 7)) basePipelineIndex


-- | Man not found. VkVertexInputBindingDescription
-- 
-- == Validaty
-- * pname:binding must: be less than sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindings
-- * pname:stride must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindingStride
data VkVertexInputBindingDescription = VkVertexInputBindingDescription
  { vvibd_binding :: !Word32
  -- ^ `binding` 
  , vvibd_stride :: !Word32
  -- ^ `stride` 
  , vvibd_inputRate :: !VkVertexInputRate
  -- ^ `inputRate` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkVertexInputBindingDescription where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    binding <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    stride <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    inputRate <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkVertexInputBindingDescription binding stride inputRate
  poke ptr (VkVertexInputBindingDescription binding stride inputRate) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) binding
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) stride
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) inputRate


-- | Man not found. VkVertexInputAttributeDescription
-- 
-- == Validaty
-- * pname:location must: be less than sname:VkPhysicalDeviceLimits::pname:maxVertexInputAttributes
-- * pname:binding must: be less than sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindings
-- * pname:offset must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxVertexInputAttributeOffset
-- * pname:format must: be allowed as a vertex buffer format, as specified by the ename:VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT flag in sname:VkFormatProperties::pname:bufferFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
data VkVertexInputAttributeDescription = VkVertexInputAttributeDescription
  { vviad_location :: !Word32
  -- ^ `location` 
  , vviad_binding :: !Word32
  -- ^ `binding` 
  , vviad_format :: !VkFormat
  -- ^ `format` 
  , vviad_offset :: !Word32
  -- ^ `offset` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkVertexInputAttributeDescription where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    location <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    binding <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    format <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    offset <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 1))
    return $ VkVertexInputAttributeDescription location binding format offset
  poke ptr (VkVertexInputAttributeDescription location binding format offset) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) location
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) binding
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) format
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 1)) offset


-- | Man not found. VkPipelineVertexInputStateCreateInfo
-- 
-- == Validaty
-- * pname:vertexBindingDescriptionCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindings
-- * pname:vertexAttributeDescriptionCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxVertexInputAttributes
-- * For every pname:binding specified by any given element of pname:pVertexAttributeDescriptions, a sname:VkVertexInputBindingDescription must: exist in pname:pVertexBindingDescriptions with the same value of pname:binding
-- * All elements of pname:pVertexBindingDescriptions must: describe distinct binding numbers
-- * All elements of pname:pVertexAttributeDescriptions must: describe distinct attribute locations
data VkPipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo
  { vpvisci_sType :: !VkStructureType
  -- ^ `sType` 
  , vpvisci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpvisci_flags :: !VkPipelineVertexInputStateCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vpvisci_vertexBindingDescriptionCount :: !Word32
  -- ^ `vertexBindingDescriptionCount`  Can be `nullPtr`.
  , vpvisci_pVertexBindingDescriptions :: !(Ptr VkVertexInputBindingDescription)
  -- ^ `pVertexBindingDescriptions`  Const. Length: vertexBindingDescriptionCount.
  , vpvisci_vertexAttributeDescriptionCount :: !Word32
  -- ^ `vertexAttributeDescriptionCount`  Can be `nullPtr`.
  , vpvisci_pVertexAttributeDescriptions :: !(Ptr VkVertexInputAttributeDescription)
  -- ^ `pVertexAttributeDescriptions`  Const. Length: vertexAttributeDescriptionCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineVertexInputStateCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    vertexBindingDescriptionCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pVertexBindingDescriptions <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    vertexAttributeDescriptionCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    pVertexAttributeDescriptions <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    return $ VkPipelineVertexInputStateCreateInfo sType pNext flags vertexBindingDescriptionCount pVertexBindingDescriptions vertexAttributeDescriptionCount pVertexAttributeDescriptions
  poke ptr (VkPipelineVertexInputStateCreateInfo sType pNext flags vertexBindingDescriptionCount pVertexBindingDescriptions vertexAttributeDescriptionCount pVertexAttributeDescriptions) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) vertexBindingDescriptionCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) pVertexBindingDescriptions
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) vertexAttributeDescriptionCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) pVertexAttributeDescriptions


-- | Man not found. VkPipelineInputAssemblyStateCreateInfo
-- 
-- == Validaty
-- * If pname:topology is ename:VK_PRIMITIVE_TOPOLOGY_POINT_LIST, ename:VK_PRIMITIVE_TOPOLOGY_LINE_LIST, ename:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, ename:VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY, ename:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY or ename:VK_PRIMITIVE_TOPOLOGY_PATCH_LIST, pname:primitiveRestartEnable must: be ename:VK_FALSE
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:topology mustnot: be any of ename:VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY, ename:VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY, ename:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY or ename:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:topology mustnot: be ename:VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
data VkPipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo
  { vpiasci_sType :: !VkStructureType
  -- ^ `sType` 
  , vpiasci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpiasci_flags :: !VkPipelineInputAssemblyStateCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vpiasci_topology :: !VkPrimitiveTopology
  -- ^ `topology` 
  , vpiasci_primitiveRestartEnable :: !VkBool32
  -- ^ `primitiveRestartEnable` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineInputAssemblyStateCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    topology <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    primitiveRestartEnable <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    return $ VkPipelineInputAssemblyStateCreateInfo sType pNext flags topology primitiveRestartEnable
  poke ptr (VkPipelineInputAssemblyStateCreateInfo sType pNext flags topology primitiveRestartEnable) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) topology
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) primitiveRestartEnable


-- | Man not found. VkPipelineTessellationStateCreateInfo
-- 
-- == Validaty
-- * pname:patchControlPoints must: be greater than zero and less than or equal to sname:VkPhysicalDeviceLimits::pname:maxTessellationPatchSize
data VkPipelineTessellationStateCreateInfo = VkPipelineTessellationStateCreateInfo
  { vptsci_sType :: !VkStructureType
  -- ^ `sType` 
  , vptsci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vptsci_flags :: !VkPipelineTessellationStateCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vptsci_patchControlPoints :: !Word32
  -- ^ `patchControlPoints` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineTessellationStateCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    patchControlPoints <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    return $ VkPipelineTessellationStateCreateInfo sType pNext flags patchControlPoints
  poke ptr (VkPipelineTessellationStateCreateInfo sType pNext flags patchControlPoints) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) patchControlPoints


-- | Man not found. VkPipelineViewportStateCreateInfo
-- 
-- == Validaty
-- * If the <<features-features-multiViewport,multiple viewports>> feature is not enabled, pname:viewportCount must: be `1`
-- * If the <<features-features-multiViewport,multiple viewports>> feature is not enabled, pname:scissorCount must: be `1`
-- * pname:viewportCount must: be between `1` and sname:VkPhysicalDeviceLimits::pname:maxViewports, inclusive
-- * pname:scissorCount must: be between `1` and sname:VkPhysicalDeviceLimits::pname:maxViewports, inclusive
-- * pname:scissorCount and pname:viewportCount must: be identical
data VkPipelineViewportStateCreateInfo = VkPipelineViewportStateCreateInfo
  { vpvsci_sType :: !VkStructureType
  -- ^ `sType` 
  , vpvsci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpvsci_flags :: !VkPipelineViewportStateCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vpvsci_viewportCount :: !Word32
  -- ^ `viewportCount` 
  , vpvsci_pViewports :: !(Ptr VkViewport)
  -- ^ `pViewports`  Can be `nullPtr`. Const. Length: viewportCount. Noautovalidity.
  , vpvsci_scissorCount :: !Word32
  -- ^ `scissorCount` 
  , vpvsci_pScissors :: !(Ptr VkRect2D)
  -- ^ `pScissors`  Can be `nullPtr`. Const. Length: scissorCount. Noautovalidity.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineViewportStateCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    viewportCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pViewports <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    scissorCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    pScissors <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    return $ VkPipelineViewportStateCreateInfo sType pNext flags viewportCount pViewports scissorCount pScissors
  poke ptr (VkPipelineViewportStateCreateInfo sType pNext flags viewportCount pViewports scissorCount pScissors) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) viewportCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) pViewports
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) scissorCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) pScissors


-- | Man not found. VkPipelineRasterizationStateCreateInfo
-- 
-- == Validaty
-- * If the <<features-features-depthClamp,depth clamping>> feature is not enabled, pname:depthClampEnable must: be ename:VK_FALSE
-- * If the <<features-features-fillModeNonSolid,non-solid fill modes>> feature is not enabled, pname:polygonMode must: be ename:VK_POLYGON_MODE_FILL
data VkPipelineRasterizationStateCreateInfo = VkPipelineRasterizationStateCreateInfo
  { vprsci_sType :: !VkStructureType
  -- ^ `sType` 
  , vprsci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vprsci_flags :: !VkPipelineRasterizationStateCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vprsci_depthClampEnable :: !VkBool32
  -- ^ `depthClampEnable` 
  , vprsci_rasterizerDiscardEnable :: !VkBool32
  -- ^ `rasterizerDiscardEnable` 
  , vprsci_polygonMode :: !VkPolygonMode
  -- ^ `polygonMode` 
  , vprsci_cullMode :: !VkCullModeFlags
  -- ^ `cullMode`  Can be `nullPtr`.
  , vprsci_frontFace :: !VkFrontFace
  -- ^ `frontFace` 
  , vprsci_depthBiasEnable :: !VkBool32
  -- ^ `depthBiasEnable` 
  , vprsci_depthBiasConstantFactor :: !Float
  -- ^ `depthBiasConstantFactor` 
  , vprsci_depthBiasClamp :: !Float
  -- ^ `depthBiasClamp` 
  , vprsci_depthBiasSlopeFactor :: !Float
  -- ^ `depthBiasSlopeFactor` 
  , vprsci_lineWidth :: !Float
  -- ^ `lineWidth` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineRasterizationStateCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 36 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    depthClampEnable <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    rasterizerDiscardEnable <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    polygonMode <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    cullMode <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    frontFace <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3))
    depthBiasEnable <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4))
    depthBiasConstantFactor <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 4))
    depthBiasClamp <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4))
    depthBiasSlopeFactor <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 4))
    lineWidth <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 4))
    return $ VkPipelineRasterizationStateCreateInfo sType pNext flags depthClampEnable rasterizerDiscardEnable polygonMode cullMode frontFace depthBiasEnable depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor lineWidth
  poke ptr (VkPipelineRasterizationStateCreateInfo sType pNext flags depthClampEnable rasterizerDiscardEnable polygonMode cullMode frontFace depthBiasEnable depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor lineWidth) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) depthClampEnable
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) rasterizerDiscardEnable
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) polygonMode
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) cullMode
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3)) frontFace
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4)) depthBiasEnable
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 4)) depthBiasConstantFactor
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4)) depthBiasClamp
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 4)) depthBiasSlopeFactor
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 4)) lineWidth


-- | Man not found. VkPipelineMultisampleStateCreateInfo
-- 
-- == Validaty
-- * If the <<features-features-sampleRateShading,sample rate shading>> feature is not enabled, pname:sampleShadingEnable must: be ename:VK_FALSE
-- * If the <<features-features-alphaToOne,alpha to one>> feature is not enabled, pname:alphaToOneEnable must: be ename:VK_FALSE
-- * pname:minSampleShading must: be in the range latexmath:[$[0,1\]$]
data VkPipelineMultisampleStateCreateInfo = VkPipelineMultisampleStateCreateInfo
  { vpmsci_sType :: !VkStructureType
  -- ^ `sType` 
  , vpmsci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpmsci_flags :: !VkPipelineMultisampleStateCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vpmsci_rasterizationSamples :: !VkSampleCountFlagBits
  -- ^ `rasterizationSamples` 
  , vpmsci_sampleShadingEnable :: !VkBool32
  -- ^ `sampleShadingEnable` 
  , vpmsci_minSampleShading :: !Float
  -- ^ `minSampleShading` 
  , vpmsci_pSampleMask :: !(Ptr VkSampleMask)
  -- ^ `pSampleMask`  Can be `nullPtr`. Const. Length: latexmath:[$\lceil{\mathit{rasterizationSamples} \over 32}\rceil$].
  , vpmsci_alphaToCoverageEnable :: !VkBool32
  -- ^ `alphaToCoverageEnable` 
  , vpmsci_alphaToOneEnable :: !VkBool32
  -- ^ `alphaToOneEnable` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineMultisampleStateCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    rasterizationSamples <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    sampleShadingEnable <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    minSampleShading <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    pSampleMask <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    alphaToCoverageEnable <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    alphaToOneEnable <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4))
    return $ VkPipelineMultisampleStateCreateInfo sType pNext flags rasterizationSamples sampleShadingEnable minSampleShading pSampleMask alphaToCoverageEnable alphaToOneEnable
  poke ptr (VkPipelineMultisampleStateCreateInfo sType pNext flags rasterizationSamples sampleShadingEnable minSampleShading pSampleMask alphaToCoverageEnable alphaToOneEnable) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) rasterizationSamples
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) sampleShadingEnable
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) minSampleShading
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) pSampleMask
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) alphaToCoverageEnable
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4)) alphaToOneEnable


-- | Man not found. VkPipelineColorBlendAttachmentState
-- 
-- == Validaty
-- * If the <<features-features-dualSrcBlend,dual source blending>> feature is not enabled, pname:srcColorBlendFactor mustnot: be ename:VK_BLEND_FACTOR_SRC1_COLOR, ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, ename:VK_BLEND_FACTOR_SRC1_ALPHA, or ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
-- * If the <<features-features-dualSrcBlend,dual source blending>> feature is not enabled, pname:dstColorBlendFactor mustnot: be ename:VK_BLEND_FACTOR_SRC1_COLOR, ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, ename:VK_BLEND_FACTOR_SRC1_ALPHA, or ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
-- * If the <<features-features-dualSrcBlend,dual source blending>> feature is not enabled, pname:srcAlphaBlendFactor mustnot: be ename:VK_BLEND_FACTOR_SRC1_COLOR, ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, ename:VK_BLEND_FACTOR_SRC1_ALPHA, or ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
-- * If the <<features-features-dualSrcBlend,dual source blending>> feature is not enabled, pname:dstAlphaBlendFactor mustnot: be ename:VK_BLEND_FACTOR_SRC1_COLOR, ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, ename:VK_BLEND_FACTOR_SRC1_ALPHA, or ename:VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
data VkPipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState
  { vpcbas_blendEnable :: !VkBool32
  -- ^ `blendEnable` 
  , vpcbas_srcColorBlendFactor :: !VkBlendFactor
  -- ^ `srcColorBlendFactor` 
  , vpcbas_dstColorBlendFactor :: !VkBlendFactor
  -- ^ `dstColorBlendFactor` 
  , vpcbas_colorBlendOp :: !VkBlendOp
  -- ^ `colorBlendOp` 
  , vpcbas_srcAlphaBlendFactor :: !VkBlendFactor
  -- ^ `srcAlphaBlendFactor` 
  , vpcbas_dstAlphaBlendFactor :: !VkBlendFactor
  -- ^ `dstAlphaBlendFactor` 
  , vpcbas_alphaBlendOp :: !VkBlendOp
  -- ^ `alphaBlendOp` 
  , vpcbas_colorWriteMask :: !VkColorComponentFlags
  -- ^ `colorWriteMask`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineColorBlendAttachmentState where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 6
  peek ptr = do
    blendEnable <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    srcColorBlendFactor <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    dstColorBlendFactor <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1))
    colorBlendOp <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    srcAlphaBlendFactor <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    dstAlphaBlendFactor <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4))
    alphaBlendOp <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 5))
    colorWriteMask <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 6))
    return $ VkPipelineColorBlendAttachmentState blendEnable srcColorBlendFactor dstColorBlendFactor colorBlendOp srcAlphaBlendFactor dstAlphaBlendFactor alphaBlendOp colorWriteMask
  poke ptr (VkPipelineColorBlendAttachmentState blendEnable srcColorBlendFactor dstColorBlendFactor colorBlendOp srcAlphaBlendFactor dstAlphaBlendFactor alphaBlendOp colorWriteMask) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) blendEnable
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) srcColorBlendFactor
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1)) dstColorBlendFactor
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) colorBlendOp
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) srcAlphaBlendFactor
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4)) dstAlphaBlendFactor
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 5)) alphaBlendOp
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 6)) colorWriteMask


-- | Man not found. VkPipelineColorBlendStateCreateInfo
-- 
-- == Validaty
-- * If the <<features-features-independentBlend,independent blending>> feature is not enabled, all elements of pname:pAttachments must: be identical
-- * If the <<features-features-logicOp,logic operations>> feature is not enabled, pname:logicOpEnable must: be ename:VK_FALSE
-- * If pname:logicOpEnable is ename:VK_TRUE, pname:logicOp must: be a valid elink:VkLogicOp value
data VkPipelineColorBlendStateCreateInfo = VkPipelineColorBlendStateCreateInfo
  { vpcbsci_sType :: !VkStructureType
  -- ^ `sType` 
  , vpcbsci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpcbsci_flags :: !VkPipelineColorBlendStateCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vpcbsci_logicOpEnable :: !VkBool32
  -- ^ `logicOpEnable` 
  , vpcbsci_logicOp :: !VkLogicOp
  -- ^ `logicOp`  Noautovalidity.
  , vpcbsci_attachmentCount :: !Word32
  -- ^ `attachmentCount`  Can be `nullPtr`.
  , vpcbsci_pAttachments :: !(Ptr VkPipelineColorBlendAttachmentState)
  -- ^ `pAttachments`  Const. Length: attachmentCount.
  , vpcbsci_blendConstants :: !(V4 Float)
  -- ^ `blendConstants`  Max: [4].
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineColorBlendStateCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 28 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    logicOpEnable <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    logicOp <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    attachmentCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    pAttachments <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    blendConstants <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    return $ VkPipelineColorBlendStateCreateInfo sType pNext flags logicOpEnable logicOp attachmentCount pAttachments blendConstants
  poke ptr (VkPipelineColorBlendStateCreateInfo sType pNext flags logicOpEnable logicOp attachmentCount pAttachments blendConstants) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) logicOpEnable
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) logicOp
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) attachmentCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) pAttachments
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) blendConstants


-- | Man not found. VkPipelineDynamicStateCreateInfo
-- 
data VkPipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo
  { vpdsci_sType :: !VkStructureType
  -- ^ `sType` 
  , vpdsci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpdsci_flags :: !VkPipelineDynamicStateCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vpdsci_dynamicStateCount :: !Word32
  -- ^ `dynamicStateCount` 
  , vpdsci_pDynamicStates :: !(Ptr VkDynamicState)
  -- ^ `pDynamicStates`  Const. Length: dynamicStateCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineDynamicStateCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    dynamicStateCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pDynamicStates <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    return $ VkPipelineDynamicStateCreateInfo sType pNext flags dynamicStateCount pDynamicStates
  poke ptr (VkPipelineDynamicStateCreateInfo sType pNext flags dynamicStateCount pDynamicStates) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) dynamicStateCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) pDynamicStates


-- | Man not found. VkStencilOpState
-- 
data VkStencilOpState = VkStencilOpState
  { vsos_failOp :: !VkStencilOp
  -- ^ `failOp` 
  , vsos_passOp :: !VkStencilOp
  -- ^ `passOp` 
  , vsos_depthFailOp :: !VkStencilOp
  -- ^ `depthFailOp` 
  , vsos_compareOp :: !VkCompareOp
  -- ^ `compareOp` 
  , vsos_compareMask :: !Word32
  -- ^ `compareMask` 
  , vsos_writeMask :: !Word32
  -- ^ `writeMask` 
  , vsos_reference :: !Word32
  -- ^ `reference` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkStencilOpState where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    failOp <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    passOp <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    depthFailOp <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    compareOp <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3))
    compareMask <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 4))
    writeMask <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4))
    reference <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4))
    return $ VkStencilOpState failOp passOp depthFailOp compareOp compareMask writeMask reference
  poke ptr (VkStencilOpState failOp passOp depthFailOp compareOp compareMask writeMask reference) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) failOp
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) passOp
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) depthFailOp
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3)) compareOp
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 4)) compareMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4)) writeMask
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4)) reference


-- | Man not found. VkPipelineDepthStencilStateCreateInfo
-- 
-- == Validaty
-- * If the <<features-features-depthBounds,depth bounds testing>> feature is not enabled, pname:depthBoundsTestEnable must: be ename:VK_FALSE
data VkPipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo
  { vpdssci_sType :: !VkStructureType
  -- ^ `sType` 
  , vpdssci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpdssci_flags :: !VkPipelineDepthStencilStateCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vpdssci_depthTestEnable :: !VkBool32
  -- ^ `depthTestEnable` 
  , vpdssci_depthWriteEnable :: !VkBool32
  -- ^ `depthWriteEnable` 
  , vpdssci_depthCompareOp :: !VkCompareOp
  -- ^ `depthCompareOp` 
  , vpdssci_depthBoundsTestEnable :: !VkBool32
  -- ^ `depthBoundsTestEnable` 
  , vpdssci_stencilTestEnable :: !VkBool32
  -- ^ `stencilTestEnable` 
  , vpdssci_front :: !VkStencilOpState
  -- ^ `front` 
  , vpdssci_back :: !VkStencilOpState
  -- ^ `back` 
  , vpdssci_minDepthBounds :: !Float
  -- ^ `minDepthBounds` 
  , vpdssci_maxDepthBounds :: !Float
  -- ^ `maxDepthBounds` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineDepthStencilStateCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 52 + sizeOf (undefined :: CSize) * 11
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    depthTestEnable <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    depthWriteEnable <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    depthCompareOp <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    depthBoundsTestEnable <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    stencilTestEnable <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3))
    front <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3))
    back <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 7))
    minDepthBounds <- peek (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 11))
    maxDepthBounds <- peek (plusPtr ptr (48 + sizeOf (undefined :: CSize) * 11))
    return $ VkPipelineDepthStencilStateCreateInfo sType pNext flags depthTestEnable depthWriteEnable depthCompareOp depthBoundsTestEnable stencilTestEnable front back minDepthBounds maxDepthBounds
  poke ptr (VkPipelineDepthStencilStateCreateInfo sType pNext flags depthTestEnable depthWriteEnable depthCompareOp depthBoundsTestEnable stencilTestEnable front back minDepthBounds maxDepthBounds) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) depthTestEnable
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) depthWriteEnable
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) depthCompareOp
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) depthBoundsTestEnable
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3)) stencilTestEnable
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3)) front
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 7)) back
    poke (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 11)) minDepthBounds
    poke (plusPtr ptr (48 + sizeOf (undefined :: CSize) * 11)) maxDepthBounds


-- | Man not found. VkGraphicsPipelineCreateInfo
-- 
-- == Validaty
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineIndex is not `-1`, pname:basePipelineHandle must: be sname:VK_NULL_HANDLE
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineIndex is not `-1`, it must: be a valid index into the calling command's pname:pCreateInfos parameter
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, pname:basePipelineIndex must: be `-1`
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, pname:basePipelineHandle must: be a valid sname:VkPipeline handle
-- * If pname:flags contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and pname:basePipelineHandle is not sname:VK_NULL_HANDLE, it must: be a valid handle to a graphics sname:VkPipeline
-- * The pname:stage member of each element of pname:pStages must: be unique
-- * The pname:stage member of one element of pname:pStages must: be ename:VK_SHADER_STAGE_VERTEX_BIT
-- * The pname:stage member of any given element of pname:pStages mustnot: be ename:VK_SHADER_STAGE_COMPUTE_BIT
-- * If pname:pStages includes a tessellation control shader stage, it must: include a tessellation evaluation shader stage
-- * If pname:pStages includes a tessellation evaluation shader stage, it must: include a tessellation control shader stage
-- * If pname:pStages includes a tessellation control shader stage and a tessellation evaluation shader stage, pname:pTessellationState mustnot: be `NULL`
-- * If pname:pStages includes both a tessellation control shader stage and a tessellation evaluation shader stage, the shader code of at least one must: contain an code:OpExecutionMode instruction that specifies the type of subdivision in the pipeline
-- * If pname:pStages includes both a tessellation control shader stage and a tessellation evaluation shader stage, and the shader code of both contain an code:OpExecutionMode instruction that specifies the type of subdivision in the pipeline, they must: both specify the same subdivision mode
-- * If pname:pStages includes both a tessellation control shader stage and a tessellation evaluation shader stage, the shader code of at least one must: contain an code:OpExecutionMode instruction that specifies the output patch size in the pipeline
-- * If pname:pStages includes both a tessellation control shader stage and a tessellation evaluation shader stage, and the shader code of both contain an code:OpExecutionMode instruction that specifies the out patch size in the pipeline, they must: both specify the same patch size
-- * If pname:pStages includes tessellation shader stages, the pname:topology member of pname:pInputAssembly must: be ename:VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
-- * If pname:pStages includes a geometry shader stage, and doesn't include any tessellation shader stages, its shader code must: contain an code:OpExecutionMode instruction that specifies an input primitive type that is <<shaders-geometry-execution, compatible>> with the primitive topology specified in pname:pInputAssembly
-- * If pname:pStages includes a geometry shader stage, and also includes tessellation shader stages, its shader code must: contain an code:OpExecutionMode instruction that specifies an input primitive type that is <<shaders-geometry-execution, compatible>> with the primitive topology that is output by the tessellation stages
-- * If pname:pStages includes a fragment shader stage and a geometry shader stage, and the fragment shader code reads from an input variable that is decorated with code:PrimitiveID, then the geometry shader code must: write to a matching output variable, decorated with code:PrimitiveID, in all execution paths
-- * If pname:pStages includes a fragment shader stage, its shader code mustnot: read from any input attachment that is defined as ename:VK_ATTACHMENT_UNUSED in pname:subpass
-- * The shader code for the entry points identified by pname:pStages, and the rest of the state identified by this structure must: adhere to the pipeline linking rules described in the <<interfaces,Shader Interfaces>> chapter
-- * If pname:subpass uses a depth/stencil attachment in pname:renderpass that has a layout of ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL in the sname:VkAttachmentReference defined by pname:subpass, and pname:pDepthStencilState is not `NULL`, the pname:depthWriteEnable member of pname:pDepthStencilState must: be ename:VK_FALSE
-- * If pname:subpass uses a depth/stencil attachment in pname:renderpass that has a layout of ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL in the sname:VkAttachmentReference defined by pname:subpass, and pname:pDepthStencilState is not `NULL`, the pname:failOp, pname:passOp and pname:depthFailOp members of each of the pname:front and pname:back members of pname:pDepthStencilState must: be ename:VK_STENCIL_OP_KEEP
-- * If pname:pColorBlendState is not `NULL`, the pname:blendEnable member of each element of the pname:pAttachment member of pname:pColorBlendState must: be ename:VK_FALSE if the pname:format of the attachment referred to in pname:subpass of pname:renderPass does not support color blend operations, as specified by the ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures or sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:pColorBlendState is not `NULL`, The pname:attachmentCount member of pname:pColorBlendState must: be equal to the pname:colorAttachmentCount used to create pname:subpass
-- * If no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_VIEWPORT, the pname:pViewports member of pname:pViewportState must: be a pointer to an array of pname:pViewportState->viewportCount sname:VkViewport structures
-- * If no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_SCISSOR, the pname:pScissors member of pname:pViewportState must: be a pointer to an array of pname:pViewportState->scissorCount sname:VkRect2D structures
-- * If the wide lines feature is not enabled, and no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_LINE_WIDTH, the pname:lineWidth member of pname:pRasterizationState must: be `1.0`
-- * If the pname:rasterizerDiscardEnable member of pname:pRasterizationState is ename:VK_FALSE, pname:pViewportState must: be a pointer to a valid sname:VkPipelineViewportStateCreateInfo structure
-- * If the pname:rasterizerDiscardEnable member of pname:pRasterizationState is ename:VK_FALSE, pname:pMultisampleState must: be a pointer to a valid sname:VkPipelineMultisampleStateCreateInfo structure
-- * If the pname:rasterizerDiscardEnable member of pname:pRasterizationState is ename:VK_FALSE, and pname:subpass uses a depth/stencil attachment, pname:pDepthStencilState must: be a pointer to a valid sname:VkPipelineDepthStencilStateCreateInfo structure
-- * If the pname:rasterizerDiscardEnable member of pname:pRasterizationState is ename:VK_FALSE, and pname:subpass uses color attachments, pname:pColorBlendState must: be a pointer to a valid sname:VkPipelineColorBlendStateCreateInfo structure
-- * If the depth bias clamping feature is not enabled, no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_DEPTH_BIAS, and the pname:depthBiasEnable member of pname:pDepthStencil is ename:VK_TRUE, the pname:depthBiasClamp member of pname:pDepthStencil must: be `0.0`
-- * If no element of the pname:pDynamicStates member of pname:pDynamicState is ename:VK_DYNAMIC_STATE_DEPTH_BOUNDS, and the pname:depthBoundsTestEnable member of pname:pDepthStencil is ename:VK_TRUE, the pname:minDepthBounds and pname:maxDepthBounds members of pname:pDepthStencil must: be between `0.0` and `1.0`, inclusive
-- * pname:layout must: be <<descriptorsets-pipelinelayout-consistency,consistent>> with all shaders specified in pname:pStages
-- * If pname:subpass uses color and/or depth/stencil attachments, then the pname:rasterizationSamples member of pname:pMultisampleState must: be the same as the sample count for those subpass attachments
-- * If pname:subpass does not use any color and/or depth/stencil attachments, then the pname:rasterizationSamples member of pname:pMultisampleState must: follow the rules for a <<renderpass-noattachments, zero-attachment subpass>>
-- * pname:subpass must: be a valid subpass within pname:renderpass
data VkGraphicsPipelineCreateInfo = VkGraphicsPipelineCreateInfo
  { vgpci_sType :: !VkStructureType
  -- ^ `sType` 
  , vgpci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vgpci_flags :: !VkPipelineCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vgpci_stageCount :: !Word32
  -- ^ `stageCount` 
  , vgpci_pStages :: !(Ptr VkPipelineShaderStageCreateInfo)
  -- ^ `pStages`  Const. Length: stageCount.
  , vgpci_pVertexInputState :: !(Ptr VkPipelineVertexInputStateCreateInfo)
  -- ^ `pVertexInputState`  Const.
  , vgpci_pInputAssemblyState :: !(Ptr VkPipelineInputAssemblyStateCreateInfo)
  -- ^ `pInputAssemblyState`  Const.
  , vgpci_pTessellationState :: !(Ptr VkPipelineTessellationStateCreateInfo)
  -- ^ `pTessellationState`  Can be `nullPtr`. Const. Noautovalidity.
  , vgpci_pViewportState :: !(Ptr VkPipelineViewportStateCreateInfo)
  -- ^ `pViewportState`  Can be `nullPtr`. Const. Noautovalidity.
  , vgpci_pRasterizationState :: !(Ptr VkPipelineRasterizationStateCreateInfo)
  -- ^ `pRasterizationState`  Const.
  , vgpci_pMultisampleState :: !(Ptr VkPipelineMultisampleStateCreateInfo)
  -- ^ `pMultisampleState`  Can be `nullPtr`. Const. Noautovalidity.
  , vgpci_pDepthStencilState :: !(Ptr VkPipelineDepthStencilStateCreateInfo)
  -- ^ `pDepthStencilState`  Can be `nullPtr`. Const. Noautovalidity.
  , vgpci_pColorBlendState :: !(Ptr VkPipelineColorBlendStateCreateInfo)
  -- ^ `pColorBlendState`  Can be `nullPtr`. Const. Noautovalidity.
  , vgpci_pDynamicState :: !(Ptr VkPipelineDynamicStateCreateInfo)
  -- ^ `pDynamicState`  Can be `nullPtr`. Const.
  , vgpci_layout :: !VkPipelineLayout
  -- ^ `layout` 
  , vgpci_renderPass :: !VkRenderPass
  -- ^ `renderPass` 
  , vgpci_subpass :: !Word32
  -- ^ `subpass` 
  , vgpci_basePipelineHandle :: !VkPipeline
  -- ^ `basePipelineHandle`  Can be `nullPtr`. Noautovalidity.
  , vgpci_basePipelineIndex :: !Int32
  -- ^ `basePipelineIndex` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkGraphicsPipelineCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 40 + sizeOf (undefined :: CSize) * 12
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    stageCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pStages <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    pVertexInputState <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    pInputAssemblyState <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4))
    pTessellationState <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 5))
    pViewportState <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 6))
    pRasterizationState <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 7))
    pMultisampleState <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 8))
    pDepthStencilState <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 9))
    pColorBlendState <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 10))
    pDynamicState <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 11))
    layout <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 12))
    renderPass <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 12))
    subpass <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 12))
    basePipelineHandle <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 12))
    basePipelineIndex <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 12))
    return $ VkGraphicsPipelineCreateInfo sType pNext flags stageCount pStages pVertexInputState pInputAssemblyState pTessellationState pViewportState pRasterizationState pMultisampleState pDepthStencilState pColorBlendState pDynamicState layout renderPass subpass basePipelineHandle basePipelineIndex
  poke ptr (VkGraphicsPipelineCreateInfo sType pNext flags stageCount pStages pVertexInputState pInputAssemblyState pTessellationState pViewportState pRasterizationState pMultisampleState pDepthStencilState pColorBlendState pDynamicState layout renderPass subpass basePipelineHandle basePipelineIndex) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) stageCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) pStages
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) pVertexInputState
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4)) pInputAssemblyState
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 5)) pTessellationState
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 6)) pViewportState
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 7)) pRasterizationState
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 8)) pMultisampleState
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 9)) pDepthStencilState
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 10)) pColorBlendState
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 11)) pDynamicState
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 12)) layout
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 12)) renderPass
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 12)) subpass
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 12)) basePipelineHandle
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 12)) basePipelineIndex


-- | Man not found. VkPipelineCacheCreateInfo
-- 
-- == Validaty
-- * If pname:initialDataSize is not `0`, it must: be equal to the size of pname:pInitialData, as returned by fname:vkGetPipelineCacheData when pname:pInitialData was originally retrieved
-- * If pname:initialDataSize is not `0`, pname:pInitialData must: have been retrieved from a previous call to fname:vkGetPipelineCacheData
data VkPipelineCacheCreateInfo = VkPipelineCacheCreateInfo
  { vpcci_sType :: !VkStructureType
  -- ^ `sType` 
  , vpcci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpcci_flags :: !VkPipelineCacheCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vpcci_initialDataSize :: !CSize
  -- ^ `initialDataSize`  Can be `nullPtr`.
  , vpcci_pInitialData :: !(Ptr ())
  -- ^ `pInitialData`  Const. Length: initialDataSize.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineCacheCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    initialDataSize <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pInitialData <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    return $ VkPipelineCacheCreateInfo sType pNext flags initialDataSize pInitialData
  poke ptr (VkPipelineCacheCreateInfo sType pNext flags initialDataSize pInitialData) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) initialDataSize
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) pInitialData


-- | Man not found. VkPushConstantRange
-- 
-- == Validaty
-- * pname:offset must: be less than sname:VkPhysicalDeviceLimits::pname:maxPushConstantsSize
-- * pname:size must: be greater than `0`
-- * pname:size must: be a multiple of `4`
-- * pname:size must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPushConstantsSize minus pname:offset
data VkPushConstantRange = VkPushConstantRange
  { vpcr_stageFlags :: !VkShaderStageFlags
  -- ^ `stageFlags` 
  , vpcr_offset :: !Word32
  -- ^ `offset` 
  , vpcr_size :: !Word32
  -- ^ `size` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPushConstantRange where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    stageFlags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    offset <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    size <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkPushConstantRange stageFlags offset size
  poke ptr (VkPushConstantRange stageFlags offset size) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) stageFlags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) offset
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) size


-- | Structure specifying the parameters of a newly created pipeline layout object..
-- Just ["Structure type. Must be ename:VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO.","Pointer to next structure in the structure chain when applicable.","Number of descriptor sets interfaced by the pipeline.","Pointer to an array of pname:setLayoutCount number of descriptor set\nlayout objects defining the layout of the descriptor set at the\ncorresponding index."]
-- Nothing
-- Just ["flink:vkCreatePipelineLayout"]
-- 
-- This structure is used to specify the parameters of pipeline layout objects created using
-- flink:vkCreatePipelineLayout.
--
-- include::../validity/structs/VkPipelineLayoutCreateInfo.txt[]
-- 
-- == Validaty
-- * pname:setLayoutCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxBoundDescriptorSets
-- * The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_SAMPLER and ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorSamplers
-- * The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER and ename:VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorUniformBuffers
-- * The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER and ename:VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorStorageBuffers
-- * The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, ename:VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE, and ename:VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorSampledImages
-- * The total number of descriptors of the type ename:VK_DESCRIPTOR_TYPE_STORAGE_IMAGE, and ename:VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER accessible to any given shader stage across all elements of pname:pSetLayouts must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPerStageDescriptorStorageImages
data VkPipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo
  { vplci_sType :: !VkStructureType
  -- ^ `sType` 
  , vplci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vplci_flags :: !VkPipelineLayoutCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vplci_setLayoutCount :: !Word32
  -- ^ `setLayoutCount`  Can be `nullPtr`.
  , vplci_pSetLayouts :: !(Ptr VkDescriptorSetLayout)
  -- ^ `pSetLayouts`  Const. Length: setLayoutCount.
  , vplci_pushConstantRangeCount :: !Word32
  -- ^ `pushConstantRangeCount`  Can be `nullPtr`.
  , vplci_pPushConstantRanges :: !(Ptr VkPushConstantRange)
  -- ^ `pPushConstantRanges`  Const. Length: pushConstantRangeCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineLayoutCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    setLayoutCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pSetLayouts <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    pushConstantRangeCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    pPushConstantRanges <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    return $ VkPipelineLayoutCreateInfo sType pNext flags setLayoutCount pSetLayouts pushConstantRangeCount pPushConstantRanges
  poke ptr (VkPipelineLayoutCreateInfo sType pNext flags setLayoutCount pSetLayouts pushConstantRangeCount pPushConstantRanges) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) setLayoutCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) pSetLayouts
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) pushConstantRangeCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) pPushConstantRanges


-- | Man not found. VkSamplerCreateInfo
-- 
-- == Validaty
-- * The absolute value of pname:mipLodBias must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxSamplerLodBias
-- * If the <<features-features-samplerAnisotropy,anisotropic sampling>> feature is not enabled, pname:anisotropyEnable must: be ename:VK_FALSE
-- * If pname:anisotropyEnable is ename:VK_TRUE, pname:maxAnisotropy must: be between `1.0` and sname:VkPhysicalDeviceLimits::pname:maxSamplerAnisotropy, inclusive
-- * If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:minFilter and pname:magFilter must: be equal
-- * If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:mipmapMode must: be ename:VK_SAMPLER_MIPMAP_MODE_NEAREST
-- * If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:minLod and pname:maxLod must: be zero
-- * If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:addressModeU and pname:addressModeV must: each be either ename:VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE or ename:VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
-- * If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:anisotropyEnable must: be ename:VK_FALSE
-- * If pname:unnormalizedCoordinates is ename:VK_TRUE, pname:compareEnable must: be ename:VK_FALSE
-- * If any of pname:addressModeU, pname:addressModeV or pname:addressModeW are ename:VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER, pname:borderColor must: be a valid elink:VkBorderColor value
-- * If the VK_KHR_mirror_clamp_to_edge extension is not enabled, pname:addressModeU, pname:addressModeV and pname:addressModeW mustnot: be ename:VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
-- * If pname:compareEnable is ename:VK_TRUE, pname:compareOp must: be a valid elink:VkCompareOp value
data VkSamplerCreateInfo = VkSamplerCreateInfo
  { vsamplerci_sType :: !VkStructureType
  -- ^ `sType` 
  , vsamplerci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vsamplerci_flags :: !VkSamplerCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vsamplerci_magFilter :: !VkFilter
  -- ^ `magFilter` 
  , vsamplerci_minFilter :: !VkFilter
  -- ^ `minFilter` 
  , vsamplerci_mipmapMode :: !VkSamplerMipmapMode
  -- ^ `mipmapMode` 
  , vsamplerci_addressModeU :: !VkSamplerAddressMode
  -- ^ `addressModeU` 
  , vsamplerci_addressModeV :: !VkSamplerAddressMode
  -- ^ `addressModeV` 
  , vsamplerci_addressModeW :: !VkSamplerAddressMode
  -- ^ `addressModeW` 
  , vsamplerci_mipLodBias :: !Float
  -- ^ `mipLodBias` 
  , vsamplerci_anisotropyEnable :: !VkBool32
  -- ^ `anisotropyEnable` 
  , vsamplerci_maxAnisotropy :: !Float
  -- ^ `maxAnisotropy` 
  , vsamplerci_compareEnable :: !VkBool32
  -- ^ `compareEnable` 
  , vsamplerci_compareOp :: !VkCompareOp
  -- ^ `compareOp`  Noautovalidity.
  , vsamplerci_minLod :: !Float
  -- ^ `minLod` 
  , vsamplerci_maxLod :: !Float
  -- ^ `maxLod` 
  , vsamplerci_borderColor :: !VkBorderColor
  -- ^ `borderColor`  Noautovalidity.
  , vsamplerci_unnormalizedCoordinates :: !VkBool32
  -- ^ `unnormalizedCoordinates` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSamplerCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 32 + sizeOf (undefined :: CSize) * 10
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    magFilter <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    minFilter <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    mipmapMode <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4))
    addressModeU <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 5))
    addressModeV <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 6))
    addressModeW <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 7))
    mipLodBias <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 8))
    anisotropyEnable <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 8))
    maxAnisotropy <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 8))
    compareEnable <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 8))
    compareOp <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 8))
    minLod <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 9))
    maxLod <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 9))
    borderColor <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 9))
    unnormalizedCoordinates <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 10))
    return $ VkSamplerCreateInfo sType pNext flags magFilter minFilter mipmapMode addressModeU addressModeV addressModeW mipLodBias anisotropyEnable maxAnisotropy compareEnable compareOp minLod maxLod borderColor unnormalizedCoordinates
  poke ptr (VkSamplerCreateInfo sType pNext flags magFilter minFilter mipmapMode addressModeU addressModeV addressModeW mipLodBias anisotropyEnable maxAnisotropy compareEnable compareOp minLod maxLod borderColor unnormalizedCoordinates) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) magFilter
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) minFilter
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4)) mipmapMode
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 5)) addressModeU
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 6)) addressModeV
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 7)) addressModeW
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 8)) mipLodBias
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 8)) anisotropyEnable
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 8)) maxAnisotropy
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 8)) compareEnable
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 8)) compareOp
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 9)) minLod
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 9)) maxLod
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 9)) borderColor
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 10)) unnormalizedCoordinates


-- | Man not found. VkCommandPoolCreateInfo
-- 
-- == Validaty
-- * pname:queueFamilyIndex must: be the index of a queue family available in the calling command's pname:device parameter
data VkCommandPoolCreateInfo = VkCommandPoolCreateInfo
  { vcpoolci_sType :: !VkStructureType
  -- ^ `sType` 
  , vcpoolci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vcpoolci_flags :: !VkCommandPoolCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vcpoolci_queueFamilyIndex :: !Word32
  -- ^ `queueFamilyIndex` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkCommandPoolCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    queueFamilyIndex <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    return $ VkCommandPoolCreateInfo sType pNext flags queueFamilyIndex
  poke ptr (VkCommandPoolCreateInfo sType pNext flags queueFamilyIndex) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) queueFamilyIndex


-- | Structure specifying the allocation parameters for command buffer object..
-- Just ["Structure type. Must be ename:VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO.","Pointer to next structure in the structure chain when applicable.","The pool from which to allocate the command buffers.","The level of the command buffers to allocate.","The numbef of command buffers to allocate."]
-- Nothing
-- Just ["flink:vkAllocateCommandBuffers"]
-- 
-- This structure is used to specify the parameters of command buffer objects allocated using
-- flink:vkAllocateCommandBuffers.
--
-- include::../validity/structs/VkCommandBufferAllocateInfo.txt[]
-- 
data VkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo
  { vcbai_sType :: !VkStructureType
  -- ^ `sType` 
  , vcbai_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vcbai_commandPool :: !VkCommandPool
  -- ^ `commandPool` 
  , vcbai_level :: !VkCommandBufferLevel
  -- ^ `level` 
  , vcbai_commandBufferCount :: !Word32
  -- ^ `commandBufferCount` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkCommandBufferAllocateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    commandPool <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    level <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    commandBufferCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    return $ VkCommandBufferAllocateInfo sType pNext commandPool level commandBufferCount
  poke ptr (VkCommandBufferAllocateInfo sType pNext commandPool level commandBufferCount) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) commandPool
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) level
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) commandBufferCount


-- | Man not found. VkCommandBufferInheritanceInfo
-- 
-- == Validaty
-- * If the <<features-features-inheritedQueries,inherited queries>> feature is not enabled, pname:occlusionQueryEnable must: be ename:VK_FALSE
-- * If the <<features-features-inheritedQueries,inherited queries>> feature is enabled, pname:queryFlags must: be a valid combination of elink:VkQueryControlFlagBits values
-- * If the <<features-features-pipelineStatisticsQuery,pipeline statistics queries>> feature is not enabled, pname:pipelineStatistics must: be code:0
data VkCommandBufferInheritanceInfo = VkCommandBufferInheritanceInfo
  { vcbii_sType :: !VkStructureType
  -- ^ `sType` 
  , vcbii_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vcbii_renderPass :: !VkRenderPass
  -- ^ `renderPass`  Can be `nullPtr`. Noautovalidity.
  , vcbii_subpass :: !Word32
  -- ^ `subpass` 
  , vcbii_framebuffer :: !VkFramebuffer
  -- ^ `framebuffer`  Can be `nullPtr`. Noautovalidity.
  , vcbii_occlusionQueryEnable :: !VkBool32
  -- ^ `occlusionQueryEnable` 
  , vcbii_queryFlags :: !VkQueryControlFlags
  -- ^ `queryFlags`  Can be `nullPtr`. Noautovalidity.
  , vcbii_pipelineStatistics :: !VkQueryPipelineStatisticFlags
  -- ^ `pipelineStatistics`  Can be `nullPtr`. Noautovalidity.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkCommandBufferInheritanceInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 32 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    renderPass <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    subpass <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    framebuffer <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    occlusionQueryEnable <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 2))
    queryFlags <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 2))
    pipelineStatistics <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 2))
    return $ VkCommandBufferInheritanceInfo sType pNext renderPass subpass framebuffer occlusionQueryEnable queryFlags pipelineStatistics
  poke ptr (VkCommandBufferInheritanceInfo sType pNext renderPass subpass framebuffer occlusionQueryEnable queryFlags pipelineStatistics) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) renderPass
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) subpass
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) framebuffer
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 2)) occlusionQueryEnable
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 2)) queryFlags
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 2)) pipelineStatistics


-- | Man not found. VkCommandBufferBeginInfo
-- 
-- == Validaty
-- * If pname:flags contains ename:VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT, the pname:renderPass member of pname:pInheritanceInfo must: be a valid sname:VkRenderPass
-- * If pname:flags contains ename:VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT, the pname:subpass member of pname:pInheritanceInfo must: be a valid subpass index within the pname:renderPass member of pname:pInheritanceInfo
-- * If pname:flags contains ename:VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT, the pname:framebuffer member of pname:pInheritanceInfo must: be either sname:VK_NULL_HANDLE, or a valid sname:VkFramebuffer that is compatible with the pname:renderPass member of pname:pInheritanceInfo
data VkCommandBufferBeginInfo = VkCommandBufferBeginInfo
  { vcbbi_sType :: !VkStructureType
  -- ^ `sType` 
  , vcbbi_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vcbbi_flags :: !VkCommandBufferUsageFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vcbbi_pInheritanceInfo :: !(Ptr VkCommandBufferInheritanceInfo)
  -- ^ `pInheritanceInfo`  Can be `nullPtr`. Const. Noautovalidity.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkCommandBufferBeginInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    pInheritanceInfo <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    return $ VkCommandBufferBeginInfo sType pNext flags pInheritanceInfo
  poke ptr (VkCommandBufferBeginInfo sType pNext flags pInheritanceInfo) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) pInheritanceInfo


-- | Man not found. VkRenderPassBeginInfo
-- 
-- == Validaty
-- * pname:clearValueCount must: be greater than or equal to the number of attachments in pname:renderPass that specify a pname:loadOp of ename:VK_ATTACHMENT_LOAD_OP_CLEAR
data VkRenderPassBeginInfo = VkRenderPassBeginInfo
  { vrpbi_sType :: !VkStructureType
  -- ^ `sType` 
  , vrpbi_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vrpbi_renderPass :: !VkRenderPass
  -- ^ `renderPass` 
  , vrpbi_framebuffer :: !VkFramebuffer
  -- ^ `framebuffer` 
  , vrpbi_renderArea :: !VkRect2D
  -- ^ `renderArea` 
  , vrpbi_clearValueCount :: !Word32
  -- ^ `clearValueCount`  Can be `nullPtr`.
  , vrpbi_pClearValues :: !(Ptr VkClearValue)
  -- ^ `pClearValues`  Const. Length: clearValueCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkRenderPassBeginInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 36 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    renderPass <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    framebuffer <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    renderArea <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    clearValueCount <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 2))
    pClearValues <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 2))
    return $ VkRenderPassBeginInfo sType pNext renderPass framebuffer renderArea clearValueCount pClearValues
  poke ptr (VkRenderPassBeginInfo sType pNext renderPass framebuffer renderArea clearValueCount pClearValues) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) renderPass
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) framebuffer
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) renderArea
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 2)) clearValueCount
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 2)) pClearValues

data VkClearColorValue = VkClearColorValueFloat32 !(V4 Float) | VkClearColorValueInt32 !(V4 Int32) | VkClearColorValueUint32 !(V4 Word32) --deriving (Eq, Show)
-- ReturnedOnly = False
instance Storable VkClearColorValue where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 0
  peek = error "cannot peek C union"
  poke ptr (VkClearColorValueFloat32 a) = poke (castPtr ptr) a
  poke ptr (VkClearColorValueInt32 a) = poke (castPtr ptr) a
  poke ptr (VkClearColorValueUint32 a) = poke (castPtr ptr) a


-- | Man not found. VkClearDepthStencilValue
-- 
data VkClearDepthStencilValue = VkClearDepthStencilValue
  { vcdsv_depth :: !Float
  -- ^ `depth` 
  , vcdsv_stencil :: !Word32
  -- ^ `stencil` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkClearDepthStencilValue where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    depth <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    stencil <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    return $ VkClearDepthStencilValue depth stencil
  poke ptr (VkClearDepthStencilValue depth stencil) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) depth
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) stencil

data VkClearValue = VkClearValueColor !VkClearColorValue | VkClearValueDepthStencil !VkClearDepthStencilValue --deriving (Eq, Show)
-- ReturnedOnly = False
instance Storable VkClearValue where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 0
  peek = error "cannot peek C union"
  poke ptr (VkClearValueColor a) = poke (castPtr ptr) a
  poke ptr (VkClearValueDepthStencil a) = poke (castPtr ptr) a


-- | Man not found. VkClearAttachment
-- 
-- == Validaty
-- * If pname:aspectMask includes ename:VK_IMAGE_ASPECT_COLOR_BIT, it mustnot: include ename:VK_IMAGE_ASPECT_DEPTH_BIT or ename:VK_IMAGE_ASPECT_STENCIL_BIT
-- * pname:aspectMask mustnot: include ename:VK_IMAGE_ASPECT_METADATA_BIT
data VkClearAttachment = VkClearAttachment
  { vca_aspectMask :: !VkImageAspectFlags
  -- ^ `aspectMask` 
  , vca_colorAttachment :: !Word32
  -- ^ `colorAttachment` 
  , vca_clearValue :: !VkClearValue
  -- ^ `clearValue` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkClearAttachment where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 24 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    aspectMask <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    colorAttachment <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    clearValue <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkClearAttachment aspectMask colorAttachment clearValue
  poke ptr (VkClearAttachment aspectMask colorAttachment clearValue) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) aspectMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) colorAttachment
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) clearValue


-- | Man not found. VkAttachmentDescription
-- 
data VkAttachmentDescription = VkAttachmentDescription
  { vad_flags :: !VkAttachmentDescriptionFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vad_format :: !VkFormat
  -- ^ `format` 
  , vad_samples :: !VkSampleCountFlagBits
  -- ^ `samples` 
  , vad_loadOp :: !VkAttachmentLoadOp
  -- ^ `loadOp` 
  , vad_storeOp :: !VkAttachmentStoreOp
  -- ^ `storeOp` 
  , vad_stencilLoadOp :: !VkAttachmentLoadOp
  -- ^ `stencilLoadOp` 
  , vad_stencilStoreOp :: !VkAttachmentStoreOp
  -- ^ `stencilStoreOp` 
  , vad_initialLayout :: !VkImageLayout
  -- ^ `initialLayout` 
  , vad_finalLayout :: !VkImageLayout
  -- ^ `finalLayout` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkAttachmentDescription where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 8
  peek ptr = do
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    format <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    samples <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1))
    loadOp <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    storeOp <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    stencilLoadOp <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4))
    stencilStoreOp <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 5))
    initialLayout <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 6))
    finalLayout <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 7))
    return $ VkAttachmentDescription flags format samples loadOp storeOp stencilLoadOp stencilStoreOp initialLayout finalLayout
  poke ptr (VkAttachmentDescription flags format samples loadOp storeOp stencilLoadOp stencilStoreOp initialLayout finalLayout) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) format
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1)) samples
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) loadOp
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) storeOp
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4)) stencilLoadOp
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 5)) stencilStoreOp
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 6)) initialLayout
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 7)) finalLayout


-- | Man not found. VkAttachmentReference
-- 
data VkAttachmentReference = VkAttachmentReference
  { var_attachment :: !Word32
  -- ^ `attachment` 
  , var_layout :: !VkImageLayout
  -- ^ `layout` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkAttachmentReference where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    attachment <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    layout <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    return $ VkAttachmentReference attachment layout
  poke ptr (VkAttachmentReference attachment layout) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) attachment
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) layout


-- | Man not found. VkSubpassDescription
-- 
-- == Validaty
-- * pname:pipelineBindPoint must: be ename:VK_PIPELINE_BIND_POINT_GRAPHICS
-- * pname:colorCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxColorAttachments
-- * If the first use of an attachment in this render pass is as an input attachment, and the attachment is not also used as a color or depth/stencil attachment in the same subpass, then pname:loadOp mustnot: be ename:VK_ATTACHMENT_LOAD_OP_CLEAR
-- * If pname:pResolveAttachments is not `NULL`, for each resolve attachment that does not have the value ename:VK_ATTACHMENT_UNUSED, the corresponding color attachment mustnot: have the value ename:VK_ATTACHMENT_UNUSED
-- * If pname:pResolveAttachments is not `NULL`, the sample count of each element of pname:pColorAttachments must: be anything other than ename:VK_SAMPLE_COUNT_1_BIT
-- * Any given element of pname:pResolveAttachments must: have a sample count of ename:VK_SAMPLE_COUNT_1_BIT
-- * Any given element of pname:pResolveAttachments must: have the same elink:VkFormat as its corresponding color attachment
-- * All attachments in pname:pColorAttachments and pname:pDepthStencilAttachment that are not ename:VK_ATTACHMENT_UNUSED must: have the same sample count
-- * If any input attachments are ename:VK_ATTACHMENT_UNUSED, then any pipelines bound during the subpass mustnot: accesss those input attachments from the fragment shader
-- * The pname:attachment member of any element of pname:pPreserveAttachments mustnot: be ename:VK_ATTACHMENT_UNUSED
-- * Any given element of pname:pPreserveAttachments mustnot: also be an element of any other member of the subpass description
-- * If any attachment is used as both an input attachment and a color or depth/stencil attachment, then each use must: use the same pname:layout
data VkSubpassDescription = VkSubpassDescription
  { vsd_flags :: !VkSubpassDescriptionFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vsd_pipelineBindPoint :: !VkPipelineBindPoint
  -- ^ `pipelineBindPoint` 
  , vsd_inputAttachmentCount :: !Word32
  -- ^ `inputAttachmentCount`  Can be `nullPtr`.
  , vsd_pInputAttachments :: !(Ptr VkAttachmentReference)
  -- ^ `pInputAttachments`  Const. Length: inputAttachmentCount.
  , vsd_colorAttachmentCount :: !Word32
  -- ^ `colorAttachmentCount`  Can be `nullPtr`.
  , vsd_pColorAttachments :: !(Ptr VkAttachmentReference)
  -- ^ `pColorAttachments`  Const. Length: colorAttachmentCount.
  , vsd_pResolveAttachments :: !(Ptr VkAttachmentReference)
  -- ^ `pResolveAttachments`  Can be `nullPtr`. Const. Length: colorAttachmentCount.
  , vsd_pDepthStencilAttachment :: !(Ptr VkAttachmentReference)
  -- ^ `pDepthStencilAttachment`  Can be `nullPtr`. Const.
  , vsd_preserveAttachmentCount :: !Word32
  -- ^ `preserveAttachmentCount`  Can be `nullPtr`.
  , vsd_pPreserveAttachments :: !(Ptr Word32)
  -- ^ `pPreserveAttachments`  Const. Length: preserveAttachmentCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSubpassDescription where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 6
  peek ptr = do
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pipelineBindPoint <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    inputAttachmentCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1))
    pInputAttachments <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 1))
    colorAttachmentCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    pColorAttachments <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    pResolveAttachments <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    pDepthStencilAttachment <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    preserveAttachmentCount <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 5))
    pPreserveAttachments <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 5))
    return $ VkSubpassDescription flags pipelineBindPoint inputAttachmentCount pInputAttachments colorAttachmentCount pColorAttachments pResolveAttachments pDepthStencilAttachment preserveAttachmentCount pPreserveAttachments
  poke ptr (VkSubpassDescription flags pipelineBindPoint inputAttachmentCount pInputAttachments colorAttachmentCount pColorAttachments pResolveAttachments pDepthStencilAttachment preserveAttachmentCount pPreserveAttachments) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) pipelineBindPoint
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 1)) inputAttachmentCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 1)) pInputAttachments
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) colorAttachmentCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) pColorAttachments
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) pResolveAttachments
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) pDepthStencilAttachment
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 5)) preserveAttachmentCount
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 5)) pPreserveAttachments


-- | Man not found. VkSubpassDependency
-- 
-- == Validaty
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:srcStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:dstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:srcStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:dstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
-- * pname:srcSubpass must: be less than or equal to pname:dstSubpass, unless one of them is ename:VK_SUBPASS_EXTERNAL, to avoid cyclic dependencies and ensure a valid execution order
-- * pname:srcSubpass and pname:dstSubpass mustnot: both be equal to ename:VK_SUBPASS_EXTERNAL
data VkSubpassDependency = VkSubpassDependency
  { vsd_srcSubpass :: !Word32
  -- ^ `srcSubpass` 
  , vsd_dstSubpass :: !Word32
  -- ^ `dstSubpass` 
  , vsd_srcStageMask :: !VkPipelineStageFlags
  -- ^ `srcStageMask` 
  , vsd_dstStageMask :: !VkPipelineStageFlags
  -- ^ `dstStageMask` 
  , vsd_srcAccessMask :: !VkAccessFlags
  -- ^ `srcAccessMask`  Can be `nullPtr`.
  , vsd_dstAccessMask :: !VkAccessFlags
  -- ^ `dstAccessMask`  Can be `nullPtr`.
  , vsd_dependencyFlags :: !VkDependencyFlags
  -- ^ `dependencyFlags`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSubpassDependency where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 28 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    srcSubpass <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    dstSubpass <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    srcStageMask <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    dstStageMask <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    srcAccessMask <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    dstAccessMask <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0))
    dependencyFlags <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0))
    return $ VkSubpassDependency srcSubpass dstSubpass srcStageMask dstStageMask srcAccessMask dstAccessMask dependencyFlags
  poke ptr (VkSubpassDependency srcSubpass dstSubpass srcStageMask dstStageMask srcAccessMask dstAccessMask dependencyFlags) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) srcSubpass
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) dstSubpass
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) srcStageMask
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) dstStageMask
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) srcAccessMask
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0)) dstAccessMask
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0)) dependencyFlags


-- | Man not found. VkRenderPassCreateInfo
-- 
-- == Validaty
-- * If any two subpasses operate on attachments with overlapping ranges of the same sname:VkDeviceMemory object, and at least one subpass writes to that area of sname:VkDeviceMemory, a subpass dependency must: be included (either directly or via some intermediate subpasses) between them
-- * If the pname:attachment member of any element of pname:pInputAttachments, pname:pColorAttachments, pname:pResolveAttachments or pname:pDepthStencilAttachment, or the attachment indexed by any element of pname:pPreserveAttachments in any given element of pname:pSubpasses is bound to a range of a sname:VkDeviceMemory object that overlaps with any other attachment in any subpass (including the same subpass), the sname:VkAttachmentDescription structures describing them must: include ename:VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT in pname:flags
-- * If the pname:attachment member of any element of pname:pInputAttachments, pname:pColorAttachments, pname:pResolveAttachments or pname:pDepthStencilAttachment, or any element of pname:pPreserveAttachments in any given element of pname:pSubpasses is not ename:VK_ATTACHMENT_UNUSED, it must: be less than pname:attachmentCount
-- * The value of any element of the pname:pPreserveAttachments member in any given element of pname:pSubpasses mustnot: be ename:VK_ATTACHMENT_UNUSED
data VkRenderPassCreateInfo = VkRenderPassCreateInfo
  { vrpci_sType :: !VkStructureType
  -- ^ `sType` 
  , vrpci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vrpci_flags :: !VkRenderPassCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vrpci_attachmentCount :: !Word32
  -- ^ `attachmentCount`  Can be `nullPtr`.
  , vrpci_pAttachments :: !(Ptr VkAttachmentDescription)
  -- ^ `pAttachments`  Const. Length: attachmentCount.
  , vrpci_subpassCount :: !Word32
  -- ^ `subpassCount` 
  , vrpci_pSubpasses :: !(Ptr VkSubpassDescription)
  -- ^ `pSubpasses`  Const. Length: subpassCount.
  , vrpci_dependencyCount :: !Word32
  -- ^ `dependencyCount`  Can be `nullPtr`.
  , vrpci_pDependencies :: !(Ptr VkSubpassDependency)
  -- ^ `pDependencies`  Const. Length: dependencyCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkRenderPassCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 5
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    attachmentCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pAttachments <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2))
    subpassCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    pSubpasses <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3))
    dependencyCount <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4))
    pDependencies <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4))
    return $ VkRenderPassCreateInfo sType pNext flags attachmentCount pAttachments subpassCount pSubpasses dependencyCount pDependencies
  poke ptr (VkRenderPassCreateInfo sType pNext flags attachmentCount pAttachments subpassCount pSubpasses dependencyCount pDependencies) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) attachmentCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 2)) pAttachments
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) subpassCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 3)) pSubpasses
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 4)) dependencyCount
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4)) pDependencies


-- | Man not found. VkEventCreateInfo
-- 
data VkEventCreateInfo = VkEventCreateInfo
  { veci_sType :: !VkStructureType
  -- ^ `sType` 
  , veci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , veci_flags :: !VkEventCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkEventCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    return $ VkEventCreateInfo sType pNext flags
  poke ptr (VkEventCreateInfo sType pNext flags) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags


-- | Man not found. VkFenceCreateInfo
-- 
data VkFenceCreateInfo = VkFenceCreateInfo
  { vfci_sType :: !VkStructureType
  -- ^ `sType` 
  , vfci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vfci_flags :: !VkFenceCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkFenceCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    return $ VkFenceCreateInfo sType pNext flags
  poke ptr (VkFenceCreateInfo sType pNext flags) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags


-- | Structure describing the fine-grained features that can be supported by an implementation..
-- Just ["out of bounds buffer accesses are well defined","full 32-bit range of indices are supported for indexed draw calls using VK_INDEX_TYPE_UINT32.","image views which are arrays of cube maps are supported (VK_IMAGE_VIEW_TYPE_CUBE_ARRAY)","blending operations are controlled independently per-attachment","geometry shader stage is supported","tessellation control and evaluation shader stages are supported","per-sample shading and multisample interpolation are supported","blend operations which take two sources are supported","logic operations are supported","multi draw indirect is supported","depth clamping is supported","depth bias clamping is supported","point and wireframe fill modes are supported","depth bounds test is supported","lines with width greater than 1 are supported","points with size greater than 1 are supported","the implementation can replace the alpha value of the color fragment output to the maximum\nrepresentable alpha value for fixed-point colors or 1.0 for floating-point colors.","mulitple viewports are supported","Anisotropic filtering is supported","ETC and EAC texture compression formats are supported","ASTC LDR texture compression formats are supported","BC1-7 texture compressed formats are supported","pipeline statistics queries are supported","storage buffers and images support stores and atomic operationscan in\nthe vertex, tessellation, and geometry shader stages.","storage buffers and images support stores and atomic operationscan in\nthe fragment shader stage.","the _PointSize_PointSize shader builtin is available in the tessellation control,\ntessellation evaluation, and geometry shader stages.","image gather with _non-constant offset_non-constant offset and image gather with _offsets_offsets are supported","the extended set of image formats can be used for storage images","multisample images can be used for storage images","arrays of uniform buffers can be accessed with dynamically uniform indices","arrays of samplers and sampled images can be accessed with dynamically uniform indices","arrays of storage buffers can be accessed with dynamically uniform indices","arrays of storage images can be accessed with dynamically uniform indices","clip distance is supported in shader code","cull distance is supported in shader code","64-bit floats (doubles) are supported in shader code","64-bit integers are supported in shader code","16-bit integers are supported in shader code","image operations that return resource residency information are supported in shader code","image operations that specify minimum resource Lod are supported in shader code","indicates whether resource memory can be managed at opaque page level","Sparse resources support: Resource memory can be managed at opaque page level rather than object level","Sparse resources support: physical device can access partially resident buffers","Sparse resources support: physical device can access partially resident 2D (non-MSAA non-DepthStencil) images","Sparse resources support: physical device can access partially resident 3D images","Sparse resources support: physical device can access partially resident MSAA 2D images with 2 samples","Sparse resources support: physical device can access partially resident MSAA 2D images with 4 samples","Sparse resources support: physical device can access partially resident MSAA 2D images with 8 samples","Sparse resources support: physical device can access partially resident MSAA 2D images with 16 samples","Sparse resources support: physical device can correctly access data aliased into multiple locations (opt-in)"]
-- Nothing
-- Just ["flink:vkGetPhysicalDeviceFeatures, flink:vkGetPhysicalDeviceProperties, flink:vkCreateDevice"]
-- 
-- The VkPhysicalDeviceFeatures structure contains a feature flag for each of
-- the fine-grained features that may be supported by an implementation.
--
-- When passed to flink:vkGetPhysicalDeviceFeatures as the ptext:pFeatures
-- parameter, the implementation will fill in each member of the structure
-- with ename:VK_TRUE if the indicated ptext:physicalDevice supports the feature,
-- or with ename:VK_FALSE if the physical device does not support the feature.
--
-- Fine-grained features must be enabled at slink:VkDevice creation time. This
-- is done by passing a pointer to a slink:VkPhysicalDeviceFeatures structure
-- in the ptext:pEnabledFeatures member of the slink:VkDeviceCreateInfo
-- structure that is passed to flink:vkCreateDevice. In this case, setting a
-- member of the structure to ename:VK_TRUE will enable support for the
-- feature on the indictated physical device, and setting a member to
-- ename:VK_FALSE will disable support for the feature.
--
-- include::../validity/structs/VkPhysicalDeviceFeatures .txt[]
-- 
-- == Validaty
-- * If any member of this structure is ename:VK_FALSE, as returned by flink:vkGetPhysicalDeviceFeatures, then it must: be ename:VK_FALSE when passed as part of the sname:VkDeviceCreateInfo struct when creating a device
data VkPhysicalDeviceFeatures = VkPhysicalDeviceFeatures
  { vpdf_robustBufferAccess :: !VkBool32
  -- ^ `robustBufferAccess` 
  , vpdf_fullDrawIndexUint32 :: !VkBool32
  -- ^ `fullDrawIndexUint32` 
  , vpdf_imageCubeArray :: !VkBool32
  -- ^ `imageCubeArray` 
  , vpdf_independentBlend :: !VkBool32
  -- ^ `independentBlend` 
  , vpdf_geometryShader :: !VkBool32
  -- ^ `geometryShader` 
  , vpdf_tessellationShader :: !VkBool32
  -- ^ `tessellationShader` 
  , vpdf_sampleRateShading :: !VkBool32
  -- ^ `sampleRateShading` 
  , vpdf_dualSrcBlend :: !VkBool32
  -- ^ `dualSrcBlend` 
  , vpdf_logicOp :: !VkBool32
  -- ^ `logicOp` 
  , vpdf_multiDrawIndirect :: !VkBool32
  -- ^ `multiDrawIndirect` 
  , vpdf_drawIndirectFirstInstance :: !VkBool32
  -- ^ `drawIndirectFirstInstance` 
  , vpdf_depthClamp :: !VkBool32
  -- ^ `depthClamp` 
  , vpdf_depthBiasClamp :: !VkBool32
  -- ^ `depthBiasClamp` 
  , vpdf_fillModeNonSolid :: !VkBool32
  -- ^ `fillModeNonSolid` 
  , vpdf_depthBounds :: !VkBool32
  -- ^ `depthBounds` 
  , vpdf_wideLines :: !VkBool32
  -- ^ `wideLines` 
  , vpdf_largePoints :: !VkBool32
  -- ^ `largePoints` 
  , vpdf_alphaToOne :: !VkBool32
  -- ^ `alphaToOne` 
  , vpdf_multiViewport :: !VkBool32
  -- ^ `multiViewport` 
  , vpdf_samplerAnisotropy :: !VkBool32
  -- ^ `samplerAnisotropy` 
  , vpdf_textureCompressionETC2 :: !VkBool32
  -- ^ `textureCompressionETC2` 
  , vpdf_textureCompressionASTC_LDR :: !VkBool32
  -- ^ `textureCompressionASTC_LDR` 
  , vpdf_textureCompressionBC :: !VkBool32
  -- ^ `textureCompressionBC` 
  , vpdf_occlusionQueryPrecise :: !VkBool32
  -- ^ `occlusionQueryPrecise` 
  , vpdf_pipelineStatisticsQuery :: !VkBool32
  -- ^ `pipelineStatisticsQuery` 
  , vpdf_vertexPipelineStoresAndAtomics :: !VkBool32
  -- ^ `vertexPipelineStoresAndAtomics` 
  , vpdf_fragmentStoresAndAtomics :: !VkBool32
  -- ^ `fragmentStoresAndAtomics` 
  , vpdf_shaderTessellationAndGeometryPointSize :: !VkBool32
  -- ^ `shaderTessellationAndGeometryPointSize` 
  , vpdf_shaderImageGatherExtended :: !VkBool32
  -- ^ `shaderImageGatherExtended` 
  , vpdf_shaderStorageImageExtendedFormats :: !VkBool32
  -- ^ `shaderStorageImageExtendedFormats` 
  , vpdf_shaderStorageImageMultisample :: !VkBool32
  -- ^ `shaderStorageImageMultisample` 
  , vpdf_shaderStorageImageReadWithoutFormat :: !VkBool32
  -- ^ `shaderStorageImageReadWithoutFormat` 
  , vpdf_shaderStorageImageWriteWithoutFormat :: !VkBool32
  -- ^ `shaderStorageImageWriteWithoutFormat` 
  , vpdf_shaderUniformBufferArrayDynamicIndexing :: !VkBool32
  -- ^ `shaderUniformBufferArrayDynamicIndexing` 
  , vpdf_shaderSampledImageArrayDynamicIndexing :: !VkBool32
  -- ^ `shaderSampledImageArrayDynamicIndexing` 
  , vpdf_shaderStorageBufferArrayDynamicIndexing :: !VkBool32
  -- ^ `shaderStorageBufferArrayDynamicIndexing` 
  , vpdf_shaderStorageImageArrayDynamicIndexing :: !VkBool32
  -- ^ `shaderStorageImageArrayDynamicIndexing` 
  , vpdf_shaderClipDistance :: !VkBool32
  -- ^ `shaderClipDistance` 
  , vpdf_shaderCullDistance :: !VkBool32
  -- ^ `shaderCullDistance` 
  , vpdf_shaderFloat64 :: !VkBool32
  -- ^ `shaderFloat64` 
  , vpdf_shaderInt64 :: !VkBool32
  -- ^ `shaderInt64` 
  , vpdf_shaderInt16 :: !VkBool32
  -- ^ `shaderInt16` 
  , vpdf_shaderResourceResidency :: !VkBool32
  -- ^ `shaderResourceResidency` 
  , vpdf_shaderResourceMinLod :: !VkBool32
  -- ^ `shaderResourceMinLod` 
  , vpdf_sparseBinding :: !VkBool32
  -- ^ `sparseBinding` 
  , vpdf_sparseResidencyBuffer :: !VkBool32
  -- ^ `sparseResidencyBuffer` 
  , vpdf_sparseResidencyImage2D :: !VkBool32
  -- ^ `sparseResidencyImage2D` 
  , vpdf_sparseResidencyImage3D :: !VkBool32
  -- ^ `sparseResidencyImage3D` 
  , vpdf_sparseResidency2Samples :: !VkBool32
  -- ^ `sparseResidency2Samples` 
  , vpdf_sparseResidency4Samples :: !VkBool32
  -- ^ `sparseResidency4Samples` 
  , vpdf_sparseResidency8Samples :: !VkBool32
  -- ^ `sparseResidency8Samples` 
  , vpdf_sparseResidency16Samples :: !VkBool32
  -- ^ `sparseResidency16Samples` 
  , vpdf_sparseResidencyAliased :: !VkBool32
  -- ^ `sparseResidencyAliased` 
  , vpdf_variableMultisampleRate :: !VkBool32
  -- ^ `variableMultisampleRate` 
  , vpdf_inheritedQueries :: !VkBool32
  -- ^ `inheritedQueries` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPhysicalDeviceFeatures where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 220 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    robustBufferAccess <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    fullDrawIndexUint32 <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    imageCubeArray <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    independentBlend <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    geometryShader <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    tessellationShader <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0))
    sampleRateShading <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0))
    dualSrcBlend <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0))
    logicOp <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0))
    multiDrawIndirect <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0))
    drawIndirectFirstInstance <- peek (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0))
    depthClamp <- peek (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0))
    depthBiasClamp <- peek (plusPtr ptr (48 + sizeOf (undefined :: CSize) * 0))
    fillModeNonSolid <- peek (plusPtr ptr (52 + sizeOf (undefined :: CSize) * 0))
    depthBounds <- peek (plusPtr ptr (56 + sizeOf (undefined :: CSize) * 0))
    wideLines <- peek (plusPtr ptr (60 + sizeOf (undefined :: CSize) * 0))
    largePoints <- peek (plusPtr ptr (64 + sizeOf (undefined :: CSize) * 0))
    alphaToOne <- peek (plusPtr ptr (68 + sizeOf (undefined :: CSize) * 0))
    multiViewport <- peek (plusPtr ptr (72 + sizeOf (undefined :: CSize) * 0))
    samplerAnisotropy <- peek (plusPtr ptr (76 + sizeOf (undefined :: CSize) * 0))
    textureCompressionETC2 <- peek (plusPtr ptr (80 + sizeOf (undefined :: CSize) * 0))
    textureCompressionASTC_LDR <- peek (plusPtr ptr (84 + sizeOf (undefined :: CSize) * 0))
    textureCompressionBC <- peek (plusPtr ptr (88 + sizeOf (undefined :: CSize) * 0))
    occlusionQueryPrecise <- peek (plusPtr ptr (92 + sizeOf (undefined :: CSize) * 0))
    pipelineStatisticsQuery <- peek (plusPtr ptr (96 + sizeOf (undefined :: CSize) * 0))
    vertexPipelineStoresAndAtomics <- peek (plusPtr ptr (100 + sizeOf (undefined :: CSize) * 0))
    fragmentStoresAndAtomics <- peek (plusPtr ptr (104 + sizeOf (undefined :: CSize) * 0))
    shaderTessellationAndGeometryPointSize <- peek (plusPtr ptr (108 + sizeOf (undefined :: CSize) * 0))
    shaderImageGatherExtended <- peek (plusPtr ptr (112 + sizeOf (undefined :: CSize) * 0))
    shaderStorageImageExtendedFormats <- peek (plusPtr ptr (116 + sizeOf (undefined :: CSize) * 0))
    shaderStorageImageMultisample <- peek (plusPtr ptr (120 + sizeOf (undefined :: CSize) * 0))
    shaderStorageImageReadWithoutFormat <- peek (plusPtr ptr (124 + sizeOf (undefined :: CSize) * 0))
    shaderStorageImageWriteWithoutFormat <- peek (plusPtr ptr (128 + sizeOf (undefined :: CSize) * 0))
    shaderUniformBufferArrayDynamicIndexing <- peek (plusPtr ptr (132 + sizeOf (undefined :: CSize) * 0))
    shaderSampledImageArrayDynamicIndexing <- peek (plusPtr ptr (136 + sizeOf (undefined :: CSize) * 0))
    shaderStorageBufferArrayDynamicIndexing <- peek (plusPtr ptr (140 + sizeOf (undefined :: CSize) * 0))
    shaderStorageImageArrayDynamicIndexing <- peek (plusPtr ptr (144 + sizeOf (undefined :: CSize) * 0))
    shaderClipDistance <- peek (plusPtr ptr (148 + sizeOf (undefined :: CSize) * 0))
    shaderCullDistance <- peek (plusPtr ptr (152 + sizeOf (undefined :: CSize) * 0))
    shaderFloat64 <- peek (plusPtr ptr (156 + sizeOf (undefined :: CSize) * 0))
    shaderInt64 <- peek (plusPtr ptr (160 + sizeOf (undefined :: CSize) * 0))
    shaderInt16 <- peek (plusPtr ptr (164 + sizeOf (undefined :: CSize) * 0))
    shaderResourceResidency <- peek (plusPtr ptr (168 + sizeOf (undefined :: CSize) * 0))
    shaderResourceMinLod <- peek (plusPtr ptr (172 + sizeOf (undefined :: CSize) * 0))
    sparseBinding <- peek (plusPtr ptr (176 + sizeOf (undefined :: CSize) * 0))
    sparseResidencyBuffer <- peek (plusPtr ptr (180 + sizeOf (undefined :: CSize) * 0))
    sparseResidencyImage2D <- peek (plusPtr ptr (184 + sizeOf (undefined :: CSize) * 0))
    sparseResidencyImage3D <- peek (plusPtr ptr (188 + sizeOf (undefined :: CSize) * 0))
    sparseResidency2Samples <- peek (plusPtr ptr (192 + sizeOf (undefined :: CSize) * 0))
    sparseResidency4Samples <- peek (plusPtr ptr (196 + sizeOf (undefined :: CSize) * 0))
    sparseResidency8Samples <- peek (plusPtr ptr (200 + sizeOf (undefined :: CSize) * 0))
    sparseResidency16Samples <- peek (plusPtr ptr (204 + sizeOf (undefined :: CSize) * 0))
    sparseResidencyAliased <- peek (plusPtr ptr (208 + sizeOf (undefined :: CSize) * 0))
    variableMultisampleRate <- peek (plusPtr ptr (212 + sizeOf (undefined :: CSize) * 0))
    inheritedQueries <- peek (plusPtr ptr (216 + sizeOf (undefined :: CSize) * 0))
    return $ VkPhysicalDeviceFeatures robustBufferAccess fullDrawIndexUint32 imageCubeArray independentBlend geometryShader tessellationShader sampleRateShading dualSrcBlend logicOp multiDrawIndirect drawIndirectFirstInstance depthClamp depthBiasClamp fillModeNonSolid depthBounds wideLines largePoints alphaToOne multiViewport samplerAnisotropy textureCompressionETC2 textureCompressionASTC_LDR textureCompressionBC occlusionQueryPrecise pipelineStatisticsQuery vertexPipelineStoresAndAtomics fragmentStoresAndAtomics shaderTessellationAndGeometryPointSize shaderImageGatherExtended shaderStorageImageExtendedFormats shaderStorageImageMultisample shaderStorageImageReadWithoutFormat shaderStorageImageWriteWithoutFormat shaderUniformBufferArrayDynamicIndexing shaderSampledImageArrayDynamicIndexing shaderStorageBufferArrayDynamicIndexing shaderStorageImageArrayDynamicIndexing shaderClipDistance shaderCullDistance shaderFloat64 shaderInt64 shaderInt16 shaderResourceResidency shaderResourceMinLod sparseBinding sparseResidencyBuffer sparseResidencyImage2D sparseResidencyImage3D sparseResidency2Samples sparseResidency4Samples sparseResidency8Samples sparseResidency16Samples sparseResidencyAliased variableMultisampleRate inheritedQueries
  poke ptr (VkPhysicalDeviceFeatures robustBufferAccess fullDrawIndexUint32 imageCubeArray independentBlend geometryShader tessellationShader sampleRateShading dualSrcBlend logicOp multiDrawIndirect drawIndirectFirstInstance depthClamp depthBiasClamp fillModeNonSolid depthBounds wideLines largePoints alphaToOne multiViewport samplerAnisotropy textureCompressionETC2 textureCompressionASTC_LDR textureCompressionBC occlusionQueryPrecise pipelineStatisticsQuery vertexPipelineStoresAndAtomics fragmentStoresAndAtomics shaderTessellationAndGeometryPointSize shaderImageGatherExtended shaderStorageImageExtendedFormats shaderStorageImageMultisample shaderStorageImageReadWithoutFormat shaderStorageImageWriteWithoutFormat shaderUniformBufferArrayDynamicIndexing shaderSampledImageArrayDynamicIndexing shaderStorageBufferArrayDynamicIndexing shaderStorageImageArrayDynamicIndexing shaderClipDistance shaderCullDistance shaderFloat64 shaderInt64 shaderInt16 shaderResourceResidency shaderResourceMinLod sparseBinding sparseResidencyBuffer sparseResidencyImage2D sparseResidencyImage3D sparseResidency2Samples sparseResidency4Samples sparseResidency8Samples sparseResidency16Samples sparseResidencyAliased variableMultisampleRate inheritedQueries) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) robustBufferAccess
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) fullDrawIndexUint32
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) imageCubeArray
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) independentBlend
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) geometryShader
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0)) tessellationShader
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0)) sampleRateShading
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0)) dualSrcBlend
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0)) logicOp
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0)) multiDrawIndirect
    poke (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0)) drawIndirectFirstInstance
    poke (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0)) depthClamp
    poke (plusPtr ptr (48 + sizeOf (undefined :: CSize) * 0)) depthBiasClamp
    poke (plusPtr ptr (52 + sizeOf (undefined :: CSize) * 0)) fillModeNonSolid
    poke (plusPtr ptr (56 + sizeOf (undefined :: CSize) * 0)) depthBounds
    poke (plusPtr ptr (60 + sizeOf (undefined :: CSize) * 0)) wideLines
    poke (plusPtr ptr (64 + sizeOf (undefined :: CSize) * 0)) largePoints
    poke (plusPtr ptr (68 + sizeOf (undefined :: CSize) * 0)) alphaToOne
    poke (plusPtr ptr (72 + sizeOf (undefined :: CSize) * 0)) multiViewport
    poke (plusPtr ptr (76 + sizeOf (undefined :: CSize) * 0)) samplerAnisotropy
    poke (plusPtr ptr (80 + sizeOf (undefined :: CSize) * 0)) textureCompressionETC2
    poke (plusPtr ptr (84 + sizeOf (undefined :: CSize) * 0)) textureCompressionASTC_LDR
    poke (plusPtr ptr (88 + sizeOf (undefined :: CSize) * 0)) textureCompressionBC
    poke (plusPtr ptr (92 + sizeOf (undefined :: CSize) * 0)) occlusionQueryPrecise
    poke (plusPtr ptr (96 + sizeOf (undefined :: CSize) * 0)) pipelineStatisticsQuery
    poke (plusPtr ptr (100 + sizeOf (undefined :: CSize) * 0)) vertexPipelineStoresAndAtomics
    poke (plusPtr ptr (104 + sizeOf (undefined :: CSize) * 0)) fragmentStoresAndAtomics
    poke (plusPtr ptr (108 + sizeOf (undefined :: CSize) * 0)) shaderTessellationAndGeometryPointSize
    poke (plusPtr ptr (112 + sizeOf (undefined :: CSize) * 0)) shaderImageGatherExtended
    poke (plusPtr ptr (116 + sizeOf (undefined :: CSize) * 0)) shaderStorageImageExtendedFormats
    poke (plusPtr ptr (120 + sizeOf (undefined :: CSize) * 0)) shaderStorageImageMultisample
    poke (plusPtr ptr (124 + sizeOf (undefined :: CSize) * 0)) shaderStorageImageReadWithoutFormat
    poke (plusPtr ptr (128 + sizeOf (undefined :: CSize) * 0)) shaderStorageImageWriteWithoutFormat
    poke (plusPtr ptr (132 + sizeOf (undefined :: CSize) * 0)) shaderUniformBufferArrayDynamicIndexing
    poke (plusPtr ptr (136 + sizeOf (undefined :: CSize) * 0)) shaderSampledImageArrayDynamicIndexing
    poke (plusPtr ptr (140 + sizeOf (undefined :: CSize) * 0)) shaderStorageBufferArrayDynamicIndexing
    poke (plusPtr ptr (144 + sizeOf (undefined :: CSize) * 0)) shaderStorageImageArrayDynamicIndexing
    poke (plusPtr ptr (148 + sizeOf (undefined :: CSize) * 0)) shaderClipDistance
    poke (plusPtr ptr (152 + sizeOf (undefined :: CSize) * 0)) shaderCullDistance
    poke (plusPtr ptr (156 + sizeOf (undefined :: CSize) * 0)) shaderFloat64
    poke (plusPtr ptr (160 + sizeOf (undefined :: CSize) * 0)) shaderInt64
    poke (plusPtr ptr (164 + sizeOf (undefined :: CSize) * 0)) shaderInt16
    poke (plusPtr ptr (168 + sizeOf (undefined :: CSize) * 0)) shaderResourceResidency
    poke (plusPtr ptr (172 + sizeOf (undefined :: CSize) * 0)) shaderResourceMinLod
    poke (plusPtr ptr (176 + sizeOf (undefined :: CSize) * 0)) sparseBinding
    poke (plusPtr ptr (180 + sizeOf (undefined :: CSize) * 0)) sparseResidencyBuffer
    poke (plusPtr ptr (184 + sizeOf (undefined :: CSize) * 0)) sparseResidencyImage2D
    poke (plusPtr ptr (188 + sizeOf (undefined :: CSize) * 0)) sparseResidencyImage3D
    poke (plusPtr ptr (192 + sizeOf (undefined :: CSize) * 0)) sparseResidency2Samples
    poke (plusPtr ptr (196 + sizeOf (undefined :: CSize) * 0)) sparseResidency4Samples
    poke (plusPtr ptr (200 + sizeOf (undefined :: CSize) * 0)) sparseResidency8Samples
    poke (plusPtr ptr (204 + sizeOf (undefined :: CSize) * 0)) sparseResidency16Samples
    poke (plusPtr ptr (208 + sizeOf (undefined :: CSize) * 0)) sparseResidencyAliased
    poke (plusPtr ptr (212 + sizeOf (undefined :: CSize) * 0)) variableMultisampleRate
    poke (plusPtr ptr (216 + sizeOf (undefined :: CSize) * 0)) inheritedQueries


-- | Man not found. VkPhysicalDeviceSparseProperties
-- 
data VkPhysicalDeviceSparseProperties = VkPhysicalDeviceSparseProperties
  { vpdsp_residencyStandard2DBlockShape :: !VkBool32
  -- ^ `residencyStandard2DBlockShape` 
  , vpdsp_residencyStandard2DMultisampleBlockShape :: !VkBool32
  -- ^ `residencyStandard2DMultisampleBlockShape` 
  , vpdsp_residencyStandard3DBlockShape :: !VkBool32
  -- ^ `residencyStandard3DBlockShape` 
  , vpdsp_residencyAlignedMipSize :: !VkBool32
  -- ^ `residencyAlignedMipSize` 
  , vpdsp_residencyNonResidentStrict :: !VkBool32
  -- ^ `residencyNonResidentStrict` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkPhysicalDeviceSparseProperties where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    residencyStandard2DBlockShape <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    residencyStandard2DMultisampleBlockShape <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    residencyStandard3DBlockShape <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    residencyAlignedMipSize <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    residencyNonResidentStrict <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    return $ VkPhysicalDeviceSparseProperties residencyStandard2DBlockShape residencyStandard2DMultisampleBlockShape residencyStandard3DBlockShape residencyAlignedMipSize residencyNonResidentStrict
  poke ptr (VkPhysicalDeviceSparseProperties residencyStandard2DBlockShape residencyStandard2DMultisampleBlockShape residencyStandard3DBlockShape residencyAlignedMipSize residencyNonResidentStrict) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) residencyStandard2DBlockShape
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) residencyStandard2DMultisampleBlockShape
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) residencyStandard3DBlockShape
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) residencyAlignedMipSize
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) residencyNonResidentStrict


-- | Structure.
-- Just ["max 1D image dimension\nmaxImageDimension2D::\nmax 2D image dimension\nmaxImageDimension3D::\nmax 3D image dimension\nmaxImageDimensionCube::\nmax cubemap image dimension\nmaxImageArrayLayers::\nmax layers for image arrays\nmaxTexelBufferSize::\nmax texel buffer size (bytes)\nmaxUniformBufferRange::\nmax uniform buffer range (bytes)\nmaxStorageBufferRange::\nmax storage buffer range (bytes)\nmaxPushConstantsSize::\nmax size of the push constants pool (bytes)","max number of device memory allocations supported\nmaxSamplerAllocationCount::\nmax number of samplers that can be allocated on a device\nbufferImageGranularity::\nGranularity (in bytes) at which buffers and linear images vs optimal\nimages can be bound to adjacent memory locations without aliasing","max number of descriptors sets that can be bound to a pipeline\nmaxPerStageDescriptorSamplers::\nmax num of samplers allowed per-stage in a descriptor set\nmaxPerStageDescriptorUniformBuffers::\nmax num of uniform buffers allowed per-stage in a descriptor set\nmaxPerStageDescriptorStorageBuffers::\nmax num of storage buffers allowed per-stage in a descriptor set\nmaxPerStageDescriptorSampledImages::\nmax num of sampled images allowed per-stage in a descriptor set\nmaxPerStageDescriptorStorageImages::\nmax num of storage images allowed per-stage in a descriptor set\nmaxPerStageDescriptorInputAttachments::\nmax num of input attachments allowed per-stage in a descriptor set\nmaxDescriptorSetUniformBuffers::\nmax num of uniform buffers allowed in all stages in a descriptor set\nmaxDescriptorSetStorageBuffers::\nmax num of storage buffers allowed in all stages in a descriptor set\nmaxDescriptorSetSampledImages::\nmax num of sampled images allowed in all stages in a descriptor set\nmaxDescriptorSetStorageImages::\nmax num of storage images allowed in all stages in a descriptor set\nmaxDescriptorSetInputAttachments::\nmax num of input attachments allowed in all stages in a descriptor set","max num of vertex input attribute slots\nmaxVertexInputBindings:\nmax num of vertex input binding slots\nmaxVertexInputAttributeOffset::\nmax vertex input attribute offset added to vertex buffer offset\nmaxVertexInputBindingStride::\nmax vertex input binding stride\nmaxVertexOutputComponents::\nmax num of output components written by vertex shader","max level supported by tessellation primitive generator\nmaxTessellationPatchSize::\nmax patch size (vertices)\nmaxTessellationControlPerVertexInputComponents::\nmax num of input components per-vertex in TCS\nmaxTessellationControlPerVertexOutputComponents::\nmax num of output components per-vertex in TCS\nmaxTessellationControlPerPatchOutputComponents::\nmax num of output components per-patch in TCS\nmaxTessellationControlTotalOutputComponents::\nmax total num of per-vertex and per-patch output components in TCS","max num of input components per vertex in TES\nmaxTessellationEvaluationOutputComponents::\nmax num of output components per vertex in TES","max invocation count supported in geometry shader\nmaxGeometryInputComponents::\nmax num of input components read in geometry stage\nmaxGeometryOutputComponents::\nmax num of output components written in geometry stage\nmaxGeometryOutputVertices::\nmax num of vertices that can be emitted in geometry stage\nmaxGeometryTotalOutputComponents::\nmax total num of components (all vertices) written in geometry stage\nmaxFragmentInputComponents::\nmax num of input compontents read in fragment stage\nmaxFragmentOutputAttachments::\nmax num of output attachments written in fragment stage\nmaxFragmentDualSourceAttachments::\nmax num of output attachments written when using dual source blending\nmaxFragmentCombinedOutputResources::\ntotal num of storage buffers, storage images and output buffers\nmaxComputeSharedMemorySize::\nmax total storage size of work group local storage (bytes)\nmaxComputeWorkGroupCount[3]::\nmax num of compute work groups that may be dispatched by a single command (x,y,z)\nmaxComputeWorkGroupInvocations::\nmax total compute invocations in a single local work group\nmaxComputeWorkGroupSize[3]::\nmax local size of a compute work group (x,y,z)","num bits of subpixel precision in screen x and y\nsubTexelPrecisionBits::\nnum bits of subtexel precision\nmipmapPrecisionBits::\nnum bits of mipmap precision","max index value for indexed draw calls (for 32-bit indices)\nmaxDrawIndirectCount::\nmax draw count for indirect draw calls","max absolute sampler level of detail bias\nmaxSamplerAnisotropy::\nmax degree of sampler anisotropy","max number of active viewports\nmaxViewportDimensions[2]::\nmax viewport dimensions (x,y)\nviewportBoundsRange[2]::\nviewport bounds range (min,max)\nviewportSubPixelBits::\nnum bits of subpixel precision for viewport","min required alignment of host-visible memory allocations within the host\naddress space (bytes)\nminTexelBufferOffsetAlignment::\nmin required alignment for texel buffer offsets (bytes)\nminUniformBufferOffsetAlignment::\nmin required alignment for uniform buffer sizes and offsets (bytes)\nminStorageBufferOffsetAlignment::\nmin required alignment for storage buffer offsets (bytes)\nminTexelOffset::\nmin texel offset for OpTextureSampleOffset\nmaxTexelOffset::\nmax texel offset for OpTextureSampleOffset\nminTexelGatherOffset::\nmin texel offset for OpTextureGatherOffset\nmaxTexelGatherOffset::\nmax texel offset for OpTextureGatherOffset\nminInterpolationOffset::\nfurthest negative offset for interpolateAtOffset\nmaxInterpolationOffset::\nfurthest positive offset for interpolateAtOffset\nsubPixelInterpolationOffsetBits::\nnum of subpixel bits for interpolateAtOffset","max width for a framebuffer\nmaxFramebufferHeight::\nmax height for a framebuffer\nmaxFramebufferLayers::\nmax layer count for a layered framebuffer\nframebufferColorSampleCounts::\nsupported color sample counts for a framebuffer\nframebufferDepthSampleCounts::\nsupported depth sample counts for a framebuffer\nframebufferStencilSampleCounts::\nsupported stencil sample counts for a framebuffer\nframebufferNoAttachmentsSampleCounts::\nsupported sample counts for a framebuffer with no attachments","max num of color attachments per subpass","supported sample counts for an image with a non-integer color format\nsampledImageIntegerSampleCounts::\nsupported sample counts for an image with an integer color format\nsampledImageDepthSampleCounts::\nsupported sample counts for an image with a depth format\nsampledImageStencilSampleCounts::\nsupported sample counts for an image with a stencil format\nstorageImageSampleCounts::\nsupported sample counts for an image used for storage operations","1/clock_tick_granularity for timestamp queries","max number of clip distances\nmaxCullDistances::\nmax number of cull distances\nmaxCombinedClipAndCullDistances::\nmax combined number of user clipping","range (min,max) of supported point sizes\nlineWidthRange[2]::\nrange (min,max) of supported line widths\npointSizeGranularity::\ngranularity of supported point sizes\nlineWidthGranularity::\ngranularity of supported line widths"]
-- Nothing
-- Just ["flink:vkGetPhysicalDeviceFeatures"]
-- 
-- include::../validity/structs/VkPhysicalDeviceLimits.txt[]
-- 
data VkPhysicalDeviceLimits = VkPhysicalDeviceLimits
  { vpdl_maxImageDimension1D :: !Word32
  -- ^ `maxImageDimension1D` 
  , vpdl_maxImageDimension2D :: !Word32
  -- ^ `maxImageDimension2D` 
  , vpdl_maxImageDimension3D :: !Word32
  -- ^ `maxImageDimension3D` 
  , vpdl_maxImageDimensionCube :: !Word32
  -- ^ `maxImageDimensionCube` 
  , vpdl_maxImageArrayLayers :: !Word32
  -- ^ `maxImageArrayLayers` 
  , vpdl_maxTexelBufferElements :: !Word32
  -- ^ `maxTexelBufferElements` 
  , vpdl_maxUniformBufferRange :: !Word32
  -- ^ `maxUniformBufferRange` 
  , vpdl_maxStorageBufferRange :: !Word32
  -- ^ `maxStorageBufferRange` 
  , vpdl_maxPushConstantsSize :: !Word32
  -- ^ `maxPushConstantsSize` 
  , vpdl_maxMemoryAllocationCount :: !Word32
  -- ^ `maxMemoryAllocationCount` 
  , vpdl_maxSamplerAllocationCount :: !Word32
  -- ^ `maxSamplerAllocationCount` 
  , vpdl_bufferImageGranularity :: !VkDeviceSize
  -- ^ `bufferImageGranularity` 
  , vpdl_sparseAddressSpaceSize :: !VkDeviceSize
  -- ^ `sparseAddressSpaceSize` 
  , vpdl_maxBoundDescriptorSets :: !Word32
  -- ^ `maxBoundDescriptorSets` 
  , vpdl_maxPerStageDescriptorSamplers :: !Word32
  -- ^ `maxPerStageDescriptorSamplers` 
  , vpdl_maxPerStageDescriptorUniformBuffers :: !Word32
  -- ^ `maxPerStageDescriptorUniformBuffers` 
  , vpdl_maxPerStageDescriptorStorageBuffers :: !Word32
  -- ^ `maxPerStageDescriptorStorageBuffers` 
  , vpdl_maxPerStageDescriptorSampledImages :: !Word32
  -- ^ `maxPerStageDescriptorSampledImages` 
  , vpdl_maxPerStageDescriptorStorageImages :: !Word32
  -- ^ `maxPerStageDescriptorStorageImages` 
  , vpdl_maxPerStageDescriptorInputAttachments :: !Word32
  -- ^ `maxPerStageDescriptorInputAttachments` 
  , vpdl_maxPerStageResources :: !Word32
  -- ^ `maxPerStageResources` 
  , vpdl_maxDescriptorSetSamplers :: !Word32
  -- ^ `maxDescriptorSetSamplers` 
  , vpdl_maxDescriptorSetUniformBuffers :: !Word32
  -- ^ `maxDescriptorSetUniformBuffers` 
  , vpdl_maxDescriptorSetUniformBuffersDynamic :: !Word32
  -- ^ `maxDescriptorSetUniformBuffersDynamic` 
  , vpdl_maxDescriptorSetStorageBuffers :: !Word32
  -- ^ `maxDescriptorSetStorageBuffers` 
  , vpdl_maxDescriptorSetStorageBuffersDynamic :: !Word32
  -- ^ `maxDescriptorSetStorageBuffersDynamic` 
  , vpdl_maxDescriptorSetSampledImages :: !Word32
  -- ^ `maxDescriptorSetSampledImages` 
  , vpdl_maxDescriptorSetStorageImages :: !Word32
  -- ^ `maxDescriptorSetStorageImages` 
  , vpdl_maxDescriptorSetInputAttachments :: !Word32
  -- ^ `maxDescriptorSetInputAttachments` 
  , vpdl_maxVertexInputAttributes :: !Word32
  -- ^ `maxVertexInputAttributes` 
  , vpdl_maxVertexInputBindings :: !Word32
  -- ^ `maxVertexInputBindings` 
  , vpdl_maxVertexInputAttributeOffset :: !Word32
  -- ^ `maxVertexInputAttributeOffset` 
  , vpdl_maxVertexInputBindingStride :: !Word32
  -- ^ `maxVertexInputBindingStride` 
  , vpdl_maxVertexOutputComponents :: !Word32
  -- ^ `maxVertexOutputComponents` 
  , vpdl_maxTessellationGenerationLevel :: !Word32
  -- ^ `maxTessellationGenerationLevel` 
  , vpdl_maxTessellationPatchSize :: !Word32
  -- ^ `maxTessellationPatchSize` 
  , vpdl_maxTessellationControlPerVertexInputComponents :: !Word32
  -- ^ `maxTessellationControlPerVertexInputComponents` 
  , vpdl_maxTessellationControlPerVertexOutputComponents :: !Word32
  -- ^ `maxTessellationControlPerVertexOutputComponents` 
  , vpdl_maxTessellationControlPerPatchOutputComponents :: !Word32
  -- ^ `maxTessellationControlPerPatchOutputComponents` 
  , vpdl_maxTessellationControlTotalOutputComponents :: !Word32
  -- ^ `maxTessellationControlTotalOutputComponents` 
  , vpdl_maxTessellationEvaluationInputComponents :: !Word32
  -- ^ `maxTessellationEvaluationInputComponents` 
  , vpdl_maxTessellationEvaluationOutputComponents :: !Word32
  -- ^ `maxTessellationEvaluationOutputComponents` 
  , vpdl_maxGeometryShaderInvocations :: !Word32
  -- ^ `maxGeometryShaderInvocations` 
  , vpdl_maxGeometryInputComponents :: !Word32
  -- ^ `maxGeometryInputComponents` 
  , vpdl_maxGeometryOutputComponents :: !Word32
  -- ^ `maxGeometryOutputComponents` 
  , vpdl_maxGeometryOutputVertices :: !Word32
  -- ^ `maxGeometryOutputVertices` 
  , vpdl_maxGeometryTotalOutputComponents :: !Word32
  -- ^ `maxGeometryTotalOutputComponents` 
  , vpdl_maxFragmentInputComponents :: !Word32
  -- ^ `maxFragmentInputComponents` 
  , vpdl_maxFragmentOutputAttachments :: !Word32
  -- ^ `maxFragmentOutputAttachments` 
  , vpdl_maxFragmentDualSrcAttachments :: !Word32
  -- ^ `maxFragmentDualSrcAttachments` 
  , vpdl_maxFragmentCombinedOutputResources :: !Word32
  -- ^ `maxFragmentCombinedOutputResources` 
  , vpdl_maxComputeSharedMemorySize :: !Word32
  -- ^ `maxComputeSharedMemorySize` 
  , vpdl_maxComputeWorkGroupCount :: !(V3 Word32)
  -- ^ `maxComputeWorkGroupCount`  Max: [3].
  , vpdl_maxComputeWorkGroupInvocations :: !Word32
  -- ^ `maxComputeWorkGroupInvocations` 
  , vpdl_maxComputeWorkGroupSize :: !(V3 Word32)
  -- ^ `maxComputeWorkGroupSize`  Max: [3].
  , vpdl_subPixelPrecisionBits :: !Word32
  -- ^ `subPixelPrecisionBits` 
  , vpdl_subTexelPrecisionBits :: !Word32
  -- ^ `subTexelPrecisionBits` 
  , vpdl_mipmapPrecisionBits :: !Word32
  -- ^ `mipmapPrecisionBits` 
  , vpdl_maxDrawIndexedIndexValue :: !Word32
  -- ^ `maxDrawIndexedIndexValue` 
  , vpdl_maxDrawIndirectCount :: !Word32
  -- ^ `maxDrawIndirectCount` 
  , vpdl_maxSamplerLodBias :: !Float
  -- ^ `maxSamplerLodBias` 
  , vpdl_maxSamplerAnisotropy :: !Float
  -- ^ `maxSamplerAnisotropy` 
  , vpdl_maxViewports :: !Word32
  -- ^ `maxViewports` 
  , vpdl_maxViewportDimensions :: !(V2 Word32)
  -- ^ `maxViewportDimensions`  Max: [2].
  , vpdl_viewportBoundsRange :: !(V2 Float)
  -- ^ `viewportBoundsRange`  Max: [2].
  , vpdl_viewportSubPixelBits :: !Word32
  -- ^ `viewportSubPixelBits` 
  , vpdl_minMemoryMapAlignment :: !CSize
  -- ^ `minMemoryMapAlignment` 
  , vpdl_minTexelBufferOffsetAlignment :: !VkDeviceSize
  -- ^ `minTexelBufferOffsetAlignment` 
  , vpdl_minUniformBufferOffsetAlignment :: !VkDeviceSize
  -- ^ `minUniformBufferOffsetAlignment` 
  , vpdl_minStorageBufferOffsetAlignment :: !VkDeviceSize
  -- ^ `minStorageBufferOffsetAlignment` 
  , vpdl_minTexelOffset :: !Int32
  -- ^ `minTexelOffset` 
  , vpdl_maxTexelOffset :: !Word32
  -- ^ `maxTexelOffset` 
  , vpdl_minTexelGatherOffset :: !Int32
  -- ^ `minTexelGatherOffset` 
  , vpdl_maxTexelGatherOffset :: !Word32
  -- ^ `maxTexelGatherOffset` 
  , vpdl_minInterpolationOffset :: !Float
  -- ^ `minInterpolationOffset` 
  , vpdl_maxInterpolationOffset :: !Float
  -- ^ `maxInterpolationOffset` 
  , vpdl_subPixelInterpolationOffsetBits :: !Word32
  -- ^ `subPixelInterpolationOffsetBits` 
  , vpdl_maxFramebufferWidth :: !Word32
  -- ^ `maxFramebufferWidth` 
  , vpdl_maxFramebufferHeight :: !Word32
  -- ^ `maxFramebufferHeight` 
  , vpdl_maxFramebufferLayers :: !Word32
  -- ^ `maxFramebufferLayers` 
  , vpdl_framebufferColorSampleCounts :: !VkSampleCountFlags
  -- ^ `framebufferColorSampleCounts`  Can be `nullPtr`.
  , vpdl_framebufferDepthSampleCounts :: !VkSampleCountFlags
  -- ^ `framebufferDepthSampleCounts`  Can be `nullPtr`.
  , vpdl_framebufferStencilSampleCounts :: !VkSampleCountFlags
  -- ^ `framebufferStencilSampleCounts`  Can be `nullPtr`.
  , vpdl_framebufferNoAttachmentsSampleCounts :: !VkSampleCountFlags
  -- ^ `framebufferNoAttachmentsSampleCounts`  Can be `nullPtr`.
  , vpdl_maxColorAttachments :: !Word32
  -- ^ `maxColorAttachments` 
  , vpdl_sampledImageColorSampleCounts :: !VkSampleCountFlags
  -- ^ `sampledImageColorSampleCounts`  Can be `nullPtr`.
  , vpdl_sampledImageIntegerSampleCounts :: !VkSampleCountFlags
  -- ^ `sampledImageIntegerSampleCounts`  Can be `nullPtr`.
  , vpdl_sampledImageDepthSampleCounts :: !VkSampleCountFlags
  -- ^ `sampledImageDepthSampleCounts`  Can be `nullPtr`.
  , vpdl_sampledImageStencilSampleCounts :: !VkSampleCountFlags
  -- ^ `sampledImageStencilSampleCounts`  Can be `nullPtr`.
  , vpdl_storageImageSampleCounts :: !VkSampleCountFlags
  -- ^ `storageImageSampleCounts`  Can be `nullPtr`.
  , vpdl_maxSampleMaskWords :: !Word32
  -- ^ `maxSampleMaskWords` 
  , vpdl_timestampComputeAndGraphics :: !VkBool32
  -- ^ `timestampComputeAndGraphics` 
  , vpdl_timestampPeriod :: !Float
  -- ^ `timestampPeriod` 
  , vpdl_maxClipDistances :: !Word32
  -- ^ `maxClipDistances` 
  , vpdl_maxCullDistances :: !Word32
  -- ^ `maxCullDistances` 
  , vpdl_maxCombinedClipAndCullDistances :: !Word32
  -- ^ `maxCombinedClipAndCullDistances` 
  , vpdl_discreteQueuePriorities :: !Word32
  -- ^ `discreteQueuePriorities` 
  , vpdl_pointSizeRange :: !(V2 Float)
  -- ^ `pointSizeRange`  Max: [2].
  , vpdl_lineWidthRange :: !(V2 Float)
  -- ^ `lineWidthRange`  Max: [2].
  , vpdl_pointSizeGranularity :: !Float
  -- ^ `pointSizeGranularity` 
  , vpdl_lineWidthGranularity :: !Float
  -- ^ `lineWidthGranularity` 
  , vpdl_strictLines :: !VkBool32
  -- ^ `strictLines` 
  , vpdl_standardSampleLocations :: !VkBool32
  -- ^ `standardSampleLocations` 
  , vpdl_optimalBufferCopyOffsetAlignment :: !VkDeviceSize
  -- ^ `optimalBufferCopyOffsetAlignment` 
  , vpdl_optimalBufferCopyRowPitchAlignment :: !VkDeviceSize
  -- ^ `optimalBufferCopyRowPitchAlignment` 
  , vpdl_nonCoherentAtomSize :: !VkDeviceSize
  -- ^ `nonCoherentAtomSize` 
  } --deriving (Eq, Show)

-- ReturnedOnly = True
instance Storable VkPhysicalDeviceLimits where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 484 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    maxImageDimension1D <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    maxImageDimension2D <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    maxImageDimension3D <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    maxImageDimensionCube <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    maxImageArrayLayers <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    maxTexelBufferElements <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0))
    maxUniformBufferRange <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0))
    maxStorageBufferRange <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0))
    maxPushConstantsSize <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0))
    maxMemoryAllocationCount <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0))
    maxSamplerAllocationCount <- peek (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0))
    bufferImageGranularity <- peek (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0))
    sparseAddressSpaceSize <- peek (plusPtr ptr (52 + sizeOf (undefined :: CSize) * 0))
    maxBoundDescriptorSets <- peek (plusPtr ptr (60 + sizeOf (undefined :: CSize) * 0))
    maxPerStageDescriptorSamplers <- peek (plusPtr ptr (64 + sizeOf (undefined :: CSize) * 0))
    maxPerStageDescriptorUniformBuffers <- peek (plusPtr ptr (68 + sizeOf (undefined :: CSize) * 0))
    maxPerStageDescriptorStorageBuffers <- peek (plusPtr ptr (72 + sizeOf (undefined :: CSize) * 0))
    maxPerStageDescriptorSampledImages <- peek (plusPtr ptr (76 + sizeOf (undefined :: CSize) * 0))
    maxPerStageDescriptorStorageImages <- peek (plusPtr ptr (80 + sizeOf (undefined :: CSize) * 0))
    maxPerStageDescriptorInputAttachments <- peek (plusPtr ptr (84 + sizeOf (undefined :: CSize) * 0))
    maxPerStageResources <- peek (plusPtr ptr (88 + sizeOf (undefined :: CSize) * 0))
    maxDescriptorSetSamplers <- peek (plusPtr ptr (92 + sizeOf (undefined :: CSize) * 0))
    maxDescriptorSetUniformBuffers <- peek (plusPtr ptr (96 + sizeOf (undefined :: CSize) * 0))
    maxDescriptorSetUniformBuffersDynamic <- peek (plusPtr ptr (100 + sizeOf (undefined :: CSize) * 0))
    maxDescriptorSetStorageBuffers <- peek (plusPtr ptr (104 + sizeOf (undefined :: CSize) * 0))
    maxDescriptorSetStorageBuffersDynamic <- peek (plusPtr ptr (108 + sizeOf (undefined :: CSize) * 0))
    maxDescriptorSetSampledImages <- peek (plusPtr ptr (112 + sizeOf (undefined :: CSize) * 0))
    maxDescriptorSetStorageImages <- peek (plusPtr ptr (116 + sizeOf (undefined :: CSize) * 0))
    maxDescriptorSetInputAttachments <- peek (plusPtr ptr (120 + sizeOf (undefined :: CSize) * 0))
    maxVertexInputAttributes <- peek (plusPtr ptr (124 + sizeOf (undefined :: CSize) * 0))
    maxVertexInputBindings <- peek (plusPtr ptr (128 + sizeOf (undefined :: CSize) * 0))
    maxVertexInputAttributeOffset <- peek (plusPtr ptr (132 + sizeOf (undefined :: CSize) * 0))
    maxVertexInputBindingStride <- peek (plusPtr ptr (136 + sizeOf (undefined :: CSize) * 0))
    maxVertexOutputComponents <- peek (plusPtr ptr (140 + sizeOf (undefined :: CSize) * 0))
    maxTessellationGenerationLevel <- peek (plusPtr ptr (144 + sizeOf (undefined :: CSize) * 0))
    maxTessellationPatchSize <- peek (plusPtr ptr (148 + sizeOf (undefined :: CSize) * 0))
    maxTessellationControlPerVertexInputComponents <- peek (plusPtr ptr (152 + sizeOf (undefined :: CSize) * 0))
    maxTessellationControlPerVertexOutputComponents <- peek (plusPtr ptr (156 + sizeOf (undefined :: CSize) * 0))
    maxTessellationControlPerPatchOutputComponents <- peek (plusPtr ptr (160 + sizeOf (undefined :: CSize) * 0))
    maxTessellationControlTotalOutputComponents <- peek (plusPtr ptr (164 + sizeOf (undefined :: CSize) * 0))
    maxTessellationEvaluationInputComponents <- peek (plusPtr ptr (168 + sizeOf (undefined :: CSize) * 0))
    maxTessellationEvaluationOutputComponents <- peek (plusPtr ptr (172 + sizeOf (undefined :: CSize) * 0))
    maxGeometryShaderInvocations <- peek (plusPtr ptr (176 + sizeOf (undefined :: CSize) * 0))
    maxGeometryInputComponents <- peek (plusPtr ptr (180 + sizeOf (undefined :: CSize) * 0))
    maxGeometryOutputComponents <- peek (plusPtr ptr (184 + sizeOf (undefined :: CSize) * 0))
    maxGeometryOutputVertices <- peek (plusPtr ptr (188 + sizeOf (undefined :: CSize) * 0))
    maxGeometryTotalOutputComponents <- peek (plusPtr ptr (192 + sizeOf (undefined :: CSize) * 0))
    maxFragmentInputComponents <- peek (plusPtr ptr (196 + sizeOf (undefined :: CSize) * 0))
    maxFragmentOutputAttachments <- peek (plusPtr ptr (200 + sizeOf (undefined :: CSize) * 0))
    maxFragmentDualSrcAttachments <- peek (plusPtr ptr (204 + sizeOf (undefined :: CSize) * 0))
    maxFragmentCombinedOutputResources <- peek (plusPtr ptr (208 + sizeOf (undefined :: CSize) * 0))
    maxComputeSharedMemorySize <- peek (plusPtr ptr (212 + sizeOf (undefined :: CSize) * 0))
    maxComputeWorkGroupCount <- peek (plusPtr ptr (216 + sizeOf (undefined :: CSize) * 0))
    maxComputeWorkGroupInvocations <- peek (plusPtr ptr (228 + sizeOf (undefined :: CSize) * 0))
    maxComputeWorkGroupSize <- peek (plusPtr ptr (232 + sizeOf (undefined :: CSize) * 0))
    subPixelPrecisionBits <- peek (plusPtr ptr (244 + sizeOf (undefined :: CSize) * 0))
    subTexelPrecisionBits <- peek (plusPtr ptr (248 + sizeOf (undefined :: CSize) * 0))
    mipmapPrecisionBits <- peek (plusPtr ptr (252 + sizeOf (undefined :: CSize) * 0))
    maxDrawIndexedIndexValue <- peek (plusPtr ptr (256 + sizeOf (undefined :: CSize) * 0))
    maxDrawIndirectCount <- peek (plusPtr ptr (260 + sizeOf (undefined :: CSize) * 0))
    maxSamplerLodBias <- peek (plusPtr ptr (264 + sizeOf (undefined :: CSize) * 0))
    maxSamplerAnisotropy <- peek (plusPtr ptr (268 + sizeOf (undefined :: CSize) * 0))
    maxViewports <- peek (plusPtr ptr (272 + sizeOf (undefined :: CSize) * 0))
    maxViewportDimensions <- peek (plusPtr ptr (276 + sizeOf (undefined :: CSize) * 0))
    viewportBoundsRange <- peek (plusPtr ptr (284 + sizeOf (undefined :: CSize) * 0))
    viewportSubPixelBits <- peek (plusPtr ptr (292 + sizeOf (undefined :: CSize) * 0))
    minMemoryMapAlignment <- peek (plusPtr ptr (296 + sizeOf (undefined :: CSize) * 0))
    minTexelBufferOffsetAlignment <- peek (plusPtr ptr (296 + sizeOf (undefined :: CSize) * 1))
    minUniformBufferOffsetAlignment <- peek (plusPtr ptr (304 + sizeOf (undefined :: CSize) * 1))
    minStorageBufferOffsetAlignment <- peek (plusPtr ptr (312 + sizeOf (undefined :: CSize) * 1))
    minTexelOffset <- peek (plusPtr ptr (320 + sizeOf (undefined :: CSize) * 1))
    maxTexelOffset <- peek (plusPtr ptr (324 + sizeOf (undefined :: CSize) * 1))
    minTexelGatherOffset <- peek (plusPtr ptr (328 + sizeOf (undefined :: CSize) * 1))
    maxTexelGatherOffset <- peek (plusPtr ptr (332 + sizeOf (undefined :: CSize) * 1))
    minInterpolationOffset <- peek (plusPtr ptr (336 + sizeOf (undefined :: CSize) * 1))
    maxInterpolationOffset <- peek (plusPtr ptr (340 + sizeOf (undefined :: CSize) * 1))
    subPixelInterpolationOffsetBits <- peek (plusPtr ptr (344 + sizeOf (undefined :: CSize) * 1))
    maxFramebufferWidth <- peek (plusPtr ptr (348 + sizeOf (undefined :: CSize) * 1))
    maxFramebufferHeight <- peek (plusPtr ptr (352 + sizeOf (undefined :: CSize) * 1))
    maxFramebufferLayers <- peek (plusPtr ptr (356 + sizeOf (undefined :: CSize) * 1))
    framebufferColorSampleCounts <- peek (plusPtr ptr (360 + sizeOf (undefined :: CSize) * 1))
    framebufferDepthSampleCounts <- peek (plusPtr ptr (364 + sizeOf (undefined :: CSize) * 1))
    framebufferStencilSampleCounts <- peek (plusPtr ptr (368 + sizeOf (undefined :: CSize) * 1))
    framebufferNoAttachmentsSampleCounts <- peek (plusPtr ptr (372 + sizeOf (undefined :: CSize) * 1))
    maxColorAttachments <- peek (plusPtr ptr (376 + sizeOf (undefined :: CSize) * 1))
    sampledImageColorSampleCounts <- peek (plusPtr ptr (380 + sizeOf (undefined :: CSize) * 1))
    sampledImageIntegerSampleCounts <- peek (plusPtr ptr (384 + sizeOf (undefined :: CSize) * 1))
    sampledImageDepthSampleCounts <- peek (plusPtr ptr (388 + sizeOf (undefined :: CSize) * 1))
    sampledImageStencilSampleCounts <- peek (plusPtr ptr (392 + sizeOf (undefined :: CSize) * 1))
    storageImageSampleCounts <- peek (plusPtr ptr (396 + sizeOf (undefined :: CSize) * 1))
    maxSampleMaskWords <- peek (plusPtr ptr (400 + sizeOf (undefined :: CSize) * 1))
    timestampComputeAndGraphics <- peek (plusPtr ptr (404 + sizeOf (undefined :: CSize) * 1))
    timestampPeriod <- peek (plusPtr ptr (408 + sizeOf (undefined :: CSize) * 1))
    maxClipDistances <- peek (plusPtr ptr (412 + sizeOf (undefined :: CSize) * 1))
    maxCullDistances <- peek (plusPtr ptr (416 + sizeOf (undefined :: CSize) * 1))
    maxCombinedClipAndCullDistances <- peek (plusPtr ptr (420 + sizeOf (undefined :: CSize) * 1))
    discreteQueuePriorities <- peek (plusPtr ptr (424 + sizeOf (undefined :: CSize) * 1))
    pointSizeRange <- peek (plusPtr ptr (428 + sizeOf (undefined :: CSize) * 1))
    lineWidthRange <- peek (plusPtr ptr (436 + sizeOf (undefined :: CSize) * 1))
    pointSizeGranularity <- peek (plusPtr ptr (444 + sizeOf (undefined :: CSize) * 1))
    lineWidthGranularity <- peek (plusPtr ptr (448 + sizeOf (undefined :: CSize) * 1))
    strictLines <- peek (plusPtr ptr (452 + sizeOf (undefined :: CSize) * 1))
    standardSampleLocations <- peek (plusPtr ptr (456 + sizeOf (undefined :: CSize) * 1))
    optimalBufferCopyOffsetAlignment <- peek (plusPtr ptr (460 + sizeOf (undefined :: CSize) * 1))
    optimalBufferCopyRowPitchAlignment <- peek (plusPtr ptr (468 + sizeOf (undefined :: CSize) * 1))
    nonCoherentAtomSize <- peek (plusPtr ptr (476 + sizeOf (undefined :: CSize) * 1))
    return $ VkPhysicalDeviceLimits maxImageDimension1D maxImageDimension2D maxImageDimension3D maxImageDimensionCube maxImageArrayLayers maxTexelBufferElements maxUniformBufferRange maxStorageBufferRange maxPushConstantsSize maxMemoryAllocationCount maxSamplerAllocationCount bufferImageGranularity sparseAddressSpaceSize maxBoundDescriptorSets maxPerStageDescriptorSamplers maxPerStageDescriptorUniformBuffers maxPerStageDescriptorStorageBuffers maxPerStageDescriptorSampledImages maxPerStageDescriptorStorageImages maxPerStageDescriptorInputAttachments maxPerStageResources maxDescriptorSetSamplers maxDescriptorSetUniformBuffers maxDescriptorSetUniformBuffersDynamic maxDescriptorSetStorageBuffers maxDescriptorSetStorageBuffersDynamic maxDescriptorSetSampledImages maxDescriptorSetStorageImages maxDescriptorSetInputAttachments maxVertexInputAttributes maxVertexInputBindings maxVertexInputAttributeOffset maxVertexInputBindingStride maxVertexOutputComponents maxTessellationGenerationLevel maxTessellationPatchSize maxTessellationControlPerVertexInputComponents maxTessellationControlPerVertexOutputComponents maxTessellationControlPerPatchOutputComponents maxTessellationControlTotalOutputComponents maxTessellationEvaluationInputComponents maxTessellationEvaluationOutputComponents maxGeometryShaderInvocations maxGeometryInputComponents maxGeometryOutputComponents maxGeometryOutputVertices maxGeometryTotalOutputComponents maxFragmentInputComponents maxFragmentOutputAttachments maxFragmentDualSrcAttachments maxFragmentCombinedOutputResources maxComputeSharedMemorySize maxComputeWorkGroupCount maxComputeWorkGroupInvocations maxComputeWorkGroupSize subPixelPrecisionBits subTexelPrecisionBits mipmapPrecisionBits maxDrawIndexedIndexValue maxDrawIndirectCount maxSamplerLodBias maxSamplerAnisotropy maxViewports maxViewportDimensions viewportBoundsRange viewportSubPixelBits minMemoryMapAlignment minTexelBufferOffsetAlignment minUniformBufferOffsetAlignment minStorageBufferOffsetAlignment minTexelOffset maxTexelOffset minTexelGatherOffset maxTexelGatherOffset minInterpolationOffset maxInterpolationOffset subPixelInterpolationOffsetBits maxFramebufferWidth maxFramebufferHeight maxFramebufferLayers framebufferColorSampleCounts framebufferDepthSampleCounts framebufferStencilSampleCounts framebufferNoAttachmentsSampleCounts maxColorAttachments sampledImageColorSampleCounts sampledImageIntegerSampleCounts sampledImageDepthSampleCounts sampledImageStencilSampleCounts storageImageSampleCounts maxSampleMaskWords timestampComputeAndGraphics timestampPeriod maxClipDistances maxCullDistances maxCombinedClipAndCullDistances discreteQueuePriorities pointSizeRange lineWidthRange pointSizeGranularity lineWidthGranularity strictLines standardSampleLocations optimalBufferCopyOffsetAlignment optimalBufferCopyRowPitchAlignment nonCoherentAtomSize
  poke ptr (VkPhysicalDeviceLimits maxImageDimension1D maxImageDimension2D maxImageDimension3D maxImageDimensionCube maxImageArrayLayers maxTexelBufferElements maxUniformBufferRange maxStorageBufferRange maxPushConstantsSize maxMemoryAllocationCount maxSamplerAllocationCount bufferImageGranularity sparseAddressSpaceSize maxBoundDescriptorSets maxPerStageDescriptorSamplers maxPerStageDescriptorUniformBuffers maxPerStageDescriptorStorageBuffers maxPerStageDescriptorSampledImages maxPerStageDescriptorStorageImages maxPerStageDescriptorInputAttachments maxPerStageResources maxDescriptorSetSamplers maxDescriptorSetUniformBuffers maxDescriptorSetUniformBuffersDynamic maxDescriptorSetStorageBuffers maxDescriptorSetStorageBuffersDynamic maxDescriptorSetSampledImages maxDescriptorSetStorageImages maxDescriptorSetInputAttachments maxVertexInputAttributes maxVertexInputBindings maxVertexInputAttributeOffset maxVertexInputBindingStride maxVertexOutputComponents maxTessellationGenerationLevel maxTessellationPatchSize maxTessellationControlPerVertexInputComponents maxTessellationControlPerVertexOutputComponents maxTessellationControlPerPatchOutputComponents maxTessellationControlTotalOutputComponents maxTessellationEvaluationInputComponents maxTessellationEvaluationOutputComponents maxGeometryShaderInvocations maxGeometryInputComponents maxGeometryOutputComponents maxGeometryOutputVertices maxGeometryTotalOutputComponents maxFragmentInputComponents maxFragmentOutputAttachments maxFragmentDualSrcAttachments maxFragmentCombinedOutputResources maxComputeSharedMemorySize maxComputeWorkGroupCount maxComputeWorkGroupInvocations maxComputeWorkGroupSize subPixelPrecisionBits subTexelPrecisionBits mipmapPrecisionBits maxDrawIndexedIndexValue maxDrawIndirectCount maxSamplerLodBias maxSamplerAnisotropy maxViewports maxViewportDimensions viewportBoundsRange viewportSubPixelBits minMemoryMapAlignment minTexelBufferOffsetAlignment minUniformBufferOffsetAlignment minStorageBufferOffsetAlignment minTexelOffset maxTexelOffset minTexelGatherOffset maxTexelGatherOffset minInterpolationOffset maxInterpolationOffset subPixelInterpolationOffsetBits maxFramebufferWidth maxFramebufferHeight maxFramebufferLayers framebufferColorSampleCounts framebufferDepthSampleCounts framebufferStencilSampleCounts framebufferNoAttachmentsSampleCounts maxColorAttachments sampledImageColorSampleCounts sampledImageIntegerSampleCounts sampledImageDepthSampleCounts sampledImageStencilSampleCounts storageImageSampleCounts maxSampleMaskWords timestampComputeAndGraphics timestampPeriod maxClipDistances maxCullDistances maxCombinedClipAndCullDistances discreteQueuePriorities pointSizeRange lineWidthRange pointSizeGranularity lineWidthGranularity strictLines standardSampleLocations optimalBufferCopyOffsetAlignment optimalBufferCopyRowPitchAlignment nonCoherentAtomSize) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) maxImageDimension1D
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) maxImageDimension2D
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) maxImageDimension3D
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) maxImageDimensionCube
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) maxImageArrayLayers
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0)) maxTexelBufferElements
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0)) maxUniformBufferRange
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0)) maxStorageBufferRange
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0)) maxPushConstantsSize
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0)) maxMemoryAllocationCount
    poke (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0)) maxSamplerAllocationCount
    poke (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0)) bufferImageGranularity
    poke (plusPtr ptr (52 + sizeOf (undefined :: CSize) * 0)) sparseAddressSpaceSize
    poke (plusPtr ptr (60 + sizeOf (undefined :: CSize) * 0)) maxBoundDescriptorSets
    poke (plusPtr ptr (64 + sizeOf (undefined :: CSize) * 0)) maxPerStageDescriptorSamplers
    poke (plusPtr ptr (68 + sizeOf (undefined :: CSize) * 0)) maxPerStageDescriptorUniformBuffers
    poke (plusPtr ptr (72 + sizeOf (undefined :: CSize) * 0)) maxPerStageDescriptorStorageBuffers
    poke (plusPtr ptr (76 + sizeOf (undefined :: CSize) * 0)) maxPerStageDescriptorSampledImages
    poke (plusPtr ptr (80 + sizeOf (undefined :: CSize) * 0)) maxPerStageDescriptorStorageImages
    poke (plusPtr ptr (84 + sizeOf (undefined :: CSize) * 0)) maxPerStageDescriptorInputAttachments
    poke (plusPtr ptr (88 + sizeOf (undefined :: CSize) * 0)) maxPerStageResources
    poke (plusPtr ptr (92 + sizeOf (undefined :: CSize) * 0)) maxDescriptorSetSamplers
    poke (plusPtr ptr (96 + sizeOf (undefined :: CSize) * 0)) maxDescriptorSetUniformBuffers
    poke (plusPtr ptr (100 + sizeOf (undefined :: CSize) * 0)) maxDescriptorSetUniformBuffersDynamic
    poke (plusPtr ptr (104 + sizeOf (undefined :: CSize) * 0)) maxDescriptorSetStorageBuffers
    poke (plusPtr ptr (108 + sizeOf (undefined :: CSize) * 0)) maxDescriptorSetStorageBuffersDynamic
    poke (plusPtr ptr (112 + sizeOf (undefined :: CSize) * 0)) maxDescriptorSetSampledImages
    poke (plusPtr ptr (116 + sizeOf (undefined :: CSize) * 0)) maxDescriptorSetStorageImages
    poke (plusPtr ptr (120 + sizeOf (undefined :: CSize) * 0)) maxDescriptorSetInputAttachments
    poke (plusPtr ptr (124 + sizeOf (undefined :: CSize) * 0)) maxVertexInputAttributes
    poke (plusPtr ptr (128 + sizeOf (undefined :: CSize) * 0)) maxVertexInputBindings
    poke (plusPtr ptr (132 + sizeOf (undefined :: CSize) * 0)) maxVertexInputAttributeOffset
    poke (plusPtr ptr (136 + sizeOf (undefined :: CSize) * 0)) maxVertexInputBindingStride
    poke (plusPtr ptr (140 + sizeOf (undefined :: CSize) * 0)) maxVertexOutputComponents
    poke (plusPtr ptr (144 + sizeOf (undefined :: CSize) * 0)) maxTessellationGenerationLevel
    poke (plusPtr ptr (148 + sizeOf (undefined :: CSize) * 0)) maxTessellationPatchSize
    poke (plusPtr ptr (152 + sizeOf (undefined :: CSize) * 0)) maxTessellationControlPerVertexInputComponents
    poke (plusPtr ptr (156 + sizeOf (undefined :: CSize) * 0)) maxTessellationControlPerVertexOutputComponents
    poke (plusPtr ptr (160 + sizeOf (undefined :: CSize) * 0)) maxTessellationControlPerPatchOutputComponents
    poke (plusPtr ptr (164 + sizeOf (undefined :: CSize) * 0)) maxTessellationControlTotalOutputComponents
    poke (plusPtr ptr (168 + sizeOf (undefined :: CSize) * 0)) maxTessellationEvaluationInputComponents
    poke (plusPtr ptr (172 + sizeOf (undefined :: CSize) * 0)) maxTessellationEvaluationOutputComponents
    poke (plusPtr ptr (176 + sizeOf (undefined :: CSize) * 0)) maxGeometryShaderInvocations
    poke (plusPtr ptr (180 + sizeOf (undefined :: CSize) * 0)) maxGeometryInputComponents
    poke (plusPtr ptr (184 + sizeOf (undefined :: CSize) * 0)) maxGeometryOutputComponents
    poke (plusPtr ptr (188 + sizeOf (undefined :: CSize) * 0)) maxGeometryOutputVertices
    poke (plusPtr ptr (192 + sizeOf (undefined :: CSize) * 0)) maxGeometryTotalOutputComponents
    poke (plusPtr ptr (196 + sizeOf (undefined :: CSize) * 0)) maxFragmentInputComponents
    poke (plusPtr ptr (200 + sizeOf (undefined :: CSize) * 0)) maxFragmentOutputAttachments
    poke (plusPtr ptr (204 + sizeOf (undefined :: CSize) * 0)) maxFragmentDualSrcAttachments
    poke (plusPtr ptr (208 + sizeOf (undefined :: CSize) * 0)) maxFragmentCombinedOutputResources
    poke (plusPtr ptr (212 + sizeOf (undefined :: CSize) * 0)) maxComputeSharedMemorySize
    poke (plusPtr ptr (216 + sizeOf (undefined :: CSize) * 0)) maxComputeWorkGroupCount
    poke (plusPtr ptr (228 + sizeOf (undefined :: CSize) * 0)) maxComputeWorkGroupInvocations
    poke (plusPtr ptr (232 + sizeOf (undefined :: CSize) * 0)) maxComputeWorkGroupSize
    poke (plusPtr ptr (244 + sizeOf (undefined :: CSize) * 0)) subPixelPrecisionBits
    poke (plusPtr ptr (248 + sizeOf (undefined :: CSize) * 0)) subTexelPrecisionBits
    poke (plusPtr ptr (252 + sizeOf (undefined :: CSize) * 0)) mipmapPrecisionBits
    poke (plusPtr ptr (256 + sizeOf (undefined :: CSize) * 0)) maxDrawIndexedIndexValue
    poke (plusPtr ptr (260 + sizeOf (undefined :: CSize) * 0)) maxDrawIndirectCount
    poke (plusPtr ptr (264 + sizeOf (undefined :: CSize) * 0)) maxSamplerLodBias
    poke (plusPtr ptr (268 + sizeOf (undefined :: CSize) * 0)) maxSamplerAnisotropy
    poke (plusPtr ptr (272 + sizeOf (undefined :: CSize) * 0)) maxViewports
    poke (plusPtr ptr (276 + sizeOf (undefined :: CSize) * 0)) maxViewportDimensions
    poke (plusPtr ptr (284 + sizeOf (undefined :: CSize) * 0)) viewportBoundsRange
    poke (plusPtr ptr (292 + sizeOf (undefined :: CSize) * 0)) viewportSubPixelBits
    poke (plusPtr ptr (296 + sizeOf (undefined :: CSize) * 0)) minMemoryMapAlignment
    poke (plusPtr ptr (296 + sizeOf (undefined :: CSize) * 1)) minTexelBufferOffsetAlignment
    poke (plusPtr ptr (304 + sizeOf (undefined :: CSize) * 1)) minUniformBufferOffsetAlignment
    poke (plusPtr ptr (312 + sizeOf (undefined :: CSize) * 1)) minStorageBufferOffsetAlignment
    poke (plusPtr ptr (320 + sizeOf (undefined :: CSize) * 1)) minTexelOffset
    poke (plusPtr ptr (324 + sizeOf (undefined :: CSize) * 1)) maxTexelOffset
    poke (plusPtr ptr (328 + sizeOf (undefined :: CSize) * 1)) minTexelGatherOffset
    poke (plusPtr ptr (332 + sizeOf (undefined :: CSize) * 1)) maxTexelGatherOffset
    poke (plusPtr ptr (336 + sizeOf (undefined :: CSize) * 1)) minInterpolationOffset
    poke (plusPtr ptr (340 + sizeOf (undefined :: CSize) * 1)) maxInterpolationOffset
    poke (plusPtr ptr (344 + sizeOf (undefined :: CSize) * 1)) subPixelInterpolationOffsetBits
    poke (plusPtr ptr (348 + sizeOf (undefined :: CSize) * 1)) maxFramebufferWidth
    poke (plusPtr ptr (352 + sizeOf (undefined :: CSize) * 1)) maxFramebufferHeight
    poke (plusPtr ptr (356 + sizeOf (undefined :: CSize) * 1)) maxFramebufferLayers
    poke (plusPtr ptr (360 + sizeOf (undefined :: CSize) * 1)) framebufferColorSampleCounts
    poke (plusPtr ptr (364 + sizeOf (undefined :: CSize) * 1)) framebufferDepthSampleCounts
    poke (plusPtr ptr (368 + sizeOf (undefined :: CSize) * 1)) framebufferStencilSampleCounts
    poke (plusPtr ptr (372 + sizeOf (undefined :: CSize) * 1)) framebufferNoAttachmentsSampleCounts
    poke (plusPtr ptr (376 + sizeOf (undefined :: CSize) * 1)) maxColorAttachments
    poke (plusPtr ptr (380 + sizeOf (undefined :: CSize) * 1)) sampledImageColorSampleCounts
    poke (plusPtr ptr (384 + sizeOf (undefined :: CSize) * 1)) sampledImageIntegerSampleCounts
    poke (plusPtr ptr (388 + sizeOf (undefined :: CSize) * 1)) sampledImageDepthSampleCounts
    poke (plusPtr ptr (392 + sizeOf (undefined :: CSize) * 1)) sampledImageStencilSampleCounts
    poke (plusPtr ptr (396 + sizeOf (undefined :: CSize) * 1)) storageImageSampleCounts
    poke (plusPtr ptr (400 + sizeOf (undefined :: CSize) * 1)) maxSampleMaskWords
    poke (plusPtr ptr (404 + sizeOf (undefined :: CSize) * 1)) timestampComputeAndGraphics
    poke (plusPtr ptr (408 + sizeOf (undefined :: CSize) * 1)) timestampPeriod
    poke (plusPtr ptr (412 + sizeOf (undefined :: CSize) * 1)) maxClipDistances
    poke (plusPtr ptr (416 + sizeOf (undefined :: CSize) * 1)) maxCullDistances
    poke (plusPtr ptr (420 + sizeOf (undefined :: CSize) * 1)) maxCombinedClipAndCullDistances
    poke (plusPtr ptr (424 + sizeOf (undefined :: CSize) * 1)) discreteQueuePriorities
    poke (plusPtr ptr (428 + sizeOf (undefined :: CSize) * 1)) pointSizeRange
    poke (plusPtr ptr (436 + sizeOf (undefined :: CSize) * 1)) lineWidthRange
    poke (plusPtr ptr (444 + sizeOf (undefined :: CSize) * 1)) pointSizeGranularity
    poke (plusPtr ptr (448 + sizeOf (undefined :: CSize) * 1)) lineWidthGranularity
    poke (plusPtr ptr (452 + sizeOf (undefined :: CSize) * 1)) strictLines
    poke (plusPtr ptr (456 + sizeOf (undefined :: CSize) * 1)) standardSampleLocations
    poke (plusPtr ptr (460 + sizeOf (undefined :: CSize) * 1)) optimalBufferCopyOffsetAlignment
    poke (plusPtr ptr (468 + sizeOf (undefined :: CSize) * 1)) optimalBufferCopyRowPitchAlignment
    poke (plusPtr ptr (476 + sizeOf (undefined :: CSize) * 1)) nonCoherentAtomSize


-- | Man not found. VkSemaphoreCreateInfo
-- 
data VkSemaphoreCreateInfo = VkSemaphoreCreateInfo
  { vsemaphoreci_sType :: !VkStructureType
  -- ^ `sType` 
  , vsemaphoreci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vsemaphoreci_flags :: !VkSemaphoreCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSemaphoreCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    return $ VkSemaphoreCreateInfo sType pNext flags
  poke ptr (VkSemaphoreCreateInfo sType pNext flags) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags


-- | Man not found. VkQueryPoolCreateInfo
-- 
-- == Validaty
-- * If the <<features-features-pipelineStatisticsQuery,pipeline statistics queries>> feature is not enabled, pname:queryType mustnot: be ename:VK_QUERY_TYPE_PIPELINE_STATISTICS
-- * If pname:queryType is ename:VK_QUERY_TYPE_PIPELINE_STATISTICS, pname:pipelineStatistics must: be a valid combination of elink:VkQueryPipelineStatisticFlagBits values
data VkQueryPoolCreateInfo = VkQueryPoolCreateInfo
  { vqpci_sType :: !VkStructureType
  -- ^ `sType` 
  , vqpci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vqpci_flags :: !VkQueryPoolCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vqpci_queryType :: !VkQueryType
  -- ^ `queryType` 
  , vqpci_queryCount :: !Word32
  -- ^ `queryCount` 
  , vqpci_pipelineStatistics :: !VkQueryPipelineStatisticFlags
  -- ^ `pipelineStatistics`  Can be `nullPtr`. Noautovalidity.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkQueryPoolCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    queryType <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    queryCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    pipelineStatistics <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    return $ VkQueryPoolCreateInfo sType pNext flags queryType queryCount pipelineStatistics
  poke ptr (VkQueryPoolCreateInfo sType pNext flags queryType queryCount pipelineStatistics) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) queryType
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) queryCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) pipelineStatistics


-- | Man not found. VkFramebufferCreateInfo
-- 
-- == Validaty
-- * pname:attachmentCount must: be equal to the attachment count specified in pname:renderPass
-- * Any given element of pname:pAttachments that is used as a color attachment or resolve attachment by pname:renderPass must: have been created with a pname:usage value including ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
-- * Any given element of pname:pAttachments that is used as a depth/stencil attachment by pname:renderPass must: have been created with a pname:usage value including ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
-- * Any given element of pname:pAttachments that is used as an input attachment by pname:renderPass must: have been created with a pname:usage value including ename:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
-- * Any given element of pname:pAttachments must: have been created with an elink:VkFormat value that matches the elink:VkFormat specified by the corresponding sname:VkAttachmentDescription in pname:renderPass
-- * Any given element of pname:pAttachments must: have been created with a pname:samples value that matches the pname:samples value specified by the corresponding sname:VkAttachmentDescription in pname:renderPass
-- * Any given element of pname:pAttachments must: have dimensions at least as large as the corresponding framebuffer dimension
-- * Any given element of pname:pAttachments must: only specify a single mip-level
-- * Any given element of pname:pAttachments must: have been created with the identity swizzle
-- * pname:width must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferWidth
-- * pname:height must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferHeight
-- * pname:layers must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxFramebufferLayers
data VkFramebufferCreateInfo = VkFramebufferCreateInfo
  { vfbci_sType :: !VkStructureType
  -- ^ `sType` 
  , vfbci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vfbci_flags :: !VkFramebufferCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  , vfbci_renderPass :: !VkRenderPass
  -- ^ `renderPass` 
  , vfbci_attachmentCount :: !Word32
  -- ^ `attachmentCount`  Can be `nullPtr`.
  , vfbci_pAttachments :: !(Ptr VkImageView)
  -- ^ `pAttachments`  Const. Length: attachmentCount.
  , vfbci_width :: !Word32
  -- ^ `width` 
  , vfbci_height :: !Word32
  -- ^ `height` 
  , vfbci_layers :: !Word32
  -- ^ `layers` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkFramebufferCreateInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 28 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    renderPass <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    attachmentCount <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    pAttachments <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    width <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3))
    height <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3))
    layers <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 3))
    return $ VkFramebufferCreateInfo sType pNext flags renderPass attachmentCount pAttachments width height layers
  poke ptr (VkFramebufferCreateInfo sType pNext flags renderPass attachmentCount pAttachments width height layers) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) renderPass
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) attachmentCount
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) pAttachments
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3)) width
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3)) height
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 3)) layers


-- | Man not found. VkDrawIndirectCommand
-- 
-- == Validaty
-- * For a given vertex buffer binding, any attribute data fetched must: be entirely contained within the corresponding vertex buffer binding, as described in <<fxvertex-input>>
-- * If the <<features-features-drawIndirectFirstInstance,drawIndirectFirstInstance>> feature is not enabled, pname:firstInstance must: be code:0
data VkDrawIndirectCommand = VkDrawIndirectCommand
  { vdic_vertexCount :: !Word32
  -- ^ `vertexCount` 
  , vdic_instanceCount :: !Word32
  -- ^ `instanceCount` 
  , vdic_firstVertex :: !Word32
  -- ^ `firstVertex` 
  , vdic_firstInstance :: !Word32
  -- ^ `firstInstance` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDrawIndirectCommand where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    vertexCount <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    instanceCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    firstVertex <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    firstInstance <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    return $ VkDrawIndirectCommand vertexCount instanceCount firstVertex firstInstance
  poke ptr (VkDrawIndirectCommand vertexCount instanceCount firstVertex firstInstance) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) vertexCount
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) instanceCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) firstVertex
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) firstInstance


-- | Man not found. VkDrawIndexedIndirectCommand
-- 
-- == Validaty
-- * For a given vertex buffer binding, any attribute data fetched must: be entirely contained within the corresponding vertex buffer binding, as described in <<fxvertex-input>>
-- * (pname:indexSize * (pname:firstIndex + pname:indexCount) + pname:offset) must: be less than or equal to the size of the currently bound index buffer, with pname:indexSize being based on the type specified by pname:indexType, where the index buffer, pname:indexType, and pname:offset are specified via fname:vkCmdBindIndexBuffer
-- * If the <<features-features-drawIndirectFirstInstance,drawIndirectFirstInstance>> feature is not enabled, pname:firstInstance must: be code:0
data VkDrawIndexedIndirectCommand = VkDrawIndexedIndirectCommand
  { vdiic_indexCount :: !Word32
  -- ^ `indexCount` 
  , vdiic_instanceCount :: !Word32
  -- ^ `instanceCount` 
  , vdiic_firstIndex :: !Word32
  -- ^ `firstIndex` 
  , vdiic_vertexOffset :: !Int32
  -- ^ `vertexOffset` 
  , vdiic_firstInstance :: !Word32
  -- ^ `firstInstance` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDrawIndexedIndirectCommand where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    indexCount <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    instanceCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    firstIndex <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    vertexOffset <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    firstInstance <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    return $ VkDrawIndexedIndirectCommand indexCount instanceCount firstIndex vertexOffset firstInstance
  poke ptr (VkDrawIndexedIndirectCommand indexCount instanceCount firstIndex vertexOffset firstInstance) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) indexCount
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) instanceCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) firstIndex
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) vertexOffset
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) firstInstance


-- | Man not found. VkDispatchIndirectCommand
-- 
-- == Validaty
-- * pname:x must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxComputeWorkGroupCount[0]
-- * pname:y must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxComputeWorkGroupCount[1]
-- * pname:z must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxComputeWorkGroupCount[2]
data VkDispatchIndirectCommand = VkDispatchIndirectCommand
  { vdic_x :: !Word32
  -- ^ `x` 
  , vdic_y :: !Word32
  -- ^ `y` 
  , vdic_z :: !Word32
  -- ^ `z` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDispatchIndirectCommand where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    x <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    y <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    z <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkDispatchIndirectCommand x y z
  poke ptr (VkDispatchIndirectCommand x y z) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) x
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) y
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) z


-- | Man not found. VkSubmitInfo
-- 
-- == Validaty
-- * Any given element of pname:pSignalSemaphores must: currently be unsignalled
-- * Any given element of pname:pCommandBuffers must: either have been recorded with the ename:VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT, or not currently be executing on the device
-- * Any given element of pname:pCommandBuffers must: be in the executable state
-- * If any given element of pname:pCommandBuffers contains commands that execute secondary command buffers, those secondary command buffers must: have been recorded with the ename:VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT, or not currently be executing on the device
-- * If any given element of pname:pCommandBuffers was created with ename:VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, it mustnot: have been previously submitted without re-recording that command buffer
-- * If any given element of pname:pCommandBuffers contains commands that execute secondary command buffers created with ename:VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, each such secondary command buffer mustnot: have been previously submitted without re-recording that command buffer
-- * Any given element of pname:pCommandBuffers mustnot: contain commands that execute a secondary command buffer, if that secondary command buffer has been recorded in another primary command buffer after it was recorded into this sname:VkCommandBuffer
-- * Any given element of pname:pCommandBuffers must: have been created on a sname:VkCommandPool that was created for the same queue family that the calling command's pname:queue belongs to
-- * Any given element of pname:pCommandBuffers mustnot: have been created with ename:VK_COMMAND_BUFFER_LEVEL_SECONDARY
-- * Any given element of sname:VkSemaphore in pname:pWaitSemaphores must: refer to a prior signal of that sname:VkSemaphore that won't be consumed by any other wait on that semaphore
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, any given element of pname:pWaitDstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, any given element of pname:pWaitDstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
data VkSubmitInfo = VkSubmitInfo
  { vsi_sType :: !VkStructureType
  -- ^ `sType` 
  , vsi_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vsi_waitSemaphoreCount :: !Word32
  -- ^ `waitSemaphoreCount`  Can be `nullPtr`.
  , vsi_pWaitSemaphores :: !(Ptr VkSemaphore)
  -- ^ `pWaitSemaphores`  Const. Length: waitSemaphoreCount.
  , vsi_pWaitDstStageMask :: !(Ptr VkPipelineStageFlags)
  -- ^ `pWaitDstStageMask`  Const. Length: waitSemaphoreCount.
  , vsi_commandBufferCount :: !Word32
  -- ^ `commandBufferCount`  Can be `nullPtr`.
  , vsi_pCommandBuffers :: !(Ptr VkCommandBuffer)
  -- ^ `pCommandBuffers`  Const. Length: commandBufferCount.
  , vsi_signalSemaphoreCount :: !Word32
  -- ^ `signalSemaphoreCount`  Can be `nullPtr`.
  , vsi_pSignalSemaphores :: !(Ptr VkSemaphore)
  -- ^ `pSignalSemaphores`  Const. Length: signalSemaphoreCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSubmitInfo where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 6
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    waitSemaphoreCount <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    pWaitSemaphores <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pWaitDstStageMask <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    commandBufferCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4))
    pCommandBuffers <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4))
    signalSemaphoreCount <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 5))
    pSignalSemaphores <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 5))
    return $ VkSubmitInfo sType pNext waitSemaphoreCount pWaitSemaphores pWaitDstStageMask commandBufferCount pCommandBuffers signalSemaphoreCount pSignalSemaphores
  poke ptr (VkSubmitInfo sType pNext waitSemaphoreCount pWaitSemaphores pWaitDstStageMask commandBufferCount pCommandBuffers signalSemaphoreCount pSignalSemaphores) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) waitSemaphoreCount
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) pWaitSemaphores
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) pWaitDstStageMask
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 4)) commandBufferCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4)) pCommandBuffers
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 5)) signalSemaphoreCount
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 5)) pSignalSemaphores


-- | Man not found. VkDisplayPropertiesKHR
-- 
data VkDisplayPropertiesKHR = VkDisplayPropertiesKHR
  { vdpkhr_display :: !VkDisplayKHR
  -- ^ `display` 
  , vdpkhr_displayName :: !(Ptr CChar)
  -- ^ `displayName`  Const. Length: null-terminated.
  , vdpkhr_physicalDimensions :: !VkExtent2D
  -- ^ `physicalDimensions` 
  , vdpkhr_physicalResolution :: !VkExtent2D
  -- ^ `physicalResolution` 
  , vdpkhr_supportedTransforms :: !VkSurfaceTransformFlagsKHR
  -- ^ `supportedTransforms`  Can be `nullPtr`.
  , vdpkhr_planeReorderPossible :: !VkBool32
  -- ^ `planeReorderPossible` 
  , vdpkhr_persistentContent :: !VkBool32
  -- ^ `persistentContent` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDisplayPropertiesKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 36 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    display <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    displayName <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    physicalDimensions <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 1))
    physicalResolution <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 1))
    supportedTransforms <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 1))
    planeReorderPossible <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 1))
    persistentContent <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 1))
    return $ VkDisplayPropertiesKHR display displayName physicalDimensions physicalResolution supportedTransforms planeReorderPossible persistentContent
  poke ptr (VkDisplayPropertiesKHR display displayName physicalDimensions physicalResolution supportedTransforms planeReorderPossible persistentContent) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) display
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) displayName
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 1)) physicalDimensions
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 1)) physicalResolution
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 1)) supportedTransforms
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 1)) planeReorderPossible
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 1)) persistentContent


-- | Man not found. VkDisplayPlanePropertiesKHR
-- 
data VkDisplayPlanePropertiesKHR = VkDisplayPlanePropertiesKHR
  { vdppkhr_currentDisplay :: !VkDisplayKHR
  -- ^ `currentDisplay` 
  , vdppkhr_currentStackIndex :: !Word32
  -- ^ `currentStackIndex` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDisplayPlanePropertiesKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    currentDisplay <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    currentStackIndex <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkDisplayPlanePropertiesKHR currentDisplay currentStackIndex
  poke ptr (VkDisplayPlanePropertiesKHR currentDisplay currentStackIndex) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) currentDisplay
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) currentStackIndex


-- | Man not found. VkDisplayModeParametersKHR
-- 
data VkDisplayModeParametersKHR = VkDisplayModeParametersKHR
  { vdmpkhr_visibleRegion :: !VkExtent2D
  -- ^ `visibleRegion` 
  , vdmpkhr_refreshRate :: !Word32
  -- ^ `refreshRate` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDisplayModeParametersKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 12 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    visibleRegion <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    refreshRate <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkDisplayModeParametersKHR visibleRegion refreshRate
  poke ptr (VkDisplayModeParametersKHR visibleRegion refreshRate) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) visibleRegion
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) refreshRate


-- | Man not found. VkDisplayModePropertiesKHR
-- 
data VkDisplayModePropertiesKHR = VkDisplayModePropertiesKHR
  { vdmpkhr_displayMode :: !VkDisplayModeKHR
  -- ^ `displayMode` 
  , vdmpkhr_parameters :: !VkDisplayModeParametersKHR
  -- ^ `parameters` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDisplayModePropertiesKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 20 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    displayMode <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    parameters <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    return $ VkDisplayModePropertiesKHR displayMode parameters
  poke ptr (VkDisplayModePropertiesKHR displayMode parameters) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) displayMode
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) parameters


-- | Man not found. VkDisplayModeCreateInfoKHR
-- 
-- == Validaty
-- * The pname:width and pname:height members of the pname:visibleRegion member of pname:parameters must: be greater than `0`
-- * The pname:refreshRate member of pname:parameters must: be greater than `0`
data VkDisplayModeCreateInfoKHR = VkDisplayModeCreateInfoKHR
  { vdmcikhr_sType :: !VkStructureType
  -- ^ `sType` 
  , vdmcikhr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdmcikhr_flags :: !VkDisplayModeCreateFlagsKHR
  -- ^ `flags`  Can be `nullPtr`.
  , vdmcikhr_parameters :: !VkDisplayModeParametersKHR
  -- ^ `parameters` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDisplayModeCreateInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    parameters <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    return $ VkDisplayModeCreateInfoKHR sType pNext flags parameters
  poke ptr (VkDisplayModeCreateInfoKHR sType pNext flags parameters) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) parameters


-- | Man not found. VkDisplayPlaneCapabilitiesKHR
-- 
data VkDisplayPlaneCapabilitiesKHR = VkDisplayPlaneCapabilitiesKHR
  { vdpckhr_supportedAlpha :: !VkDisplayPlaneAlphaFlagsKHR
  -- ^ `supportedAlpha`  Can be `nullPtr`.
  , vdpckhr_minSrcPosition :: !VkOffset2D
  -- ^ `minSrcPosition` 
  , vdpckhr_maxSrcPosition :: !VkOffset2D
  -- ^ `maxSrcPosition` 
  , vdpckhr_minSrcExtent :: !VkExtent2D
  -- ^ `minSrcExtent` 
  , vdpckhr_maxSrcExtent :: !VkExtent2D
  -- ^ `maxSrcExtent` 
  , vdpckhr_minDstPosition :: !VkOffset2D
  -- ^ `minDstPosition` 
  , vdpckhr_maxDstPosition :: !VkOffset2D
  -- ^ `maxDstPosition` 
  , vdpckhr_minDstExtent :: !VkExtent2D
  -- ^ `minDstExtent` 
  , vdpckhr_maxDstExtent :: !VkExtent2D
  -- ^ `maxDstExtent` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDisplayPlaneCapabilitiesKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 68 + sizeOf (undefined :: CSize) * 0
  peek ptr = do
    supportedAlpha <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    minSrcPosition <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    maxSrcPosition <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0))
    minSrcExtent <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0))
    maxSrcExtent <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0))
    minDstPosition <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0))
    maxDstPosition <- peek (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0))
    minDstExtent <- peek (plusPtr ptr (52 + sizeOf (undefined :: CSize) * 0))
    maxDstExtent <- peek (plusPtr ptr (60 + sizeOf (undefined :: CSize) * 0))
    return $ VkDisplayPlaneCapabilitiesKHR supportedAlpha minSrcPosition maxSrcPosition minSrcExtent maxSrcExtent minDstPosition maxDstPosition minDstExtent maxDstExtent
  poke ptr (VkDisplayPlaneCapabilitiesKHR supportedAlpha minSrcPosition maxSrcPosition minSrcExtent maxSrcExtent minDstPosition maxDstPosition minDstExtent maxDstExtent) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) supportedAlpha
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) minSrcPosition
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 0)) maxSrcPosition
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 0)) minSrcExtent
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 0)) maxSrcExtent
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0)) minDstPosition
    poke (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 0)) maxDstPosition
    poke (plusPtr ptr (52 + sizeOf (undefined :: CSize) * 0)) minDstExtent
    poke (plusPtr ptr (60 + sizeOf (undefined :: CSize) * 0)) maxDstExtent


-- | Man not found. VkDisplaySurfaceCreateInfoKHR
-- 
-- == Validaty
-- * pname:planeIndex must: be less than the number of display planes supported by the device as determined by calling fname:vkGetPhysicalDeviceDisplayPlanePropertiesKHR
-- * If the pname:planeReorderPossible member of the sname:VkDisplayPropertiesKHR structure returned by fname:vkGetPhysicalDeviceDisplayPropertiesKHR for the display corresponding to pname:displayMode is ename:VK_TRUE then pname:planeStackIndex must: be less than the number of display planes supported by the device as determined by calling fname:vkGetPhysicalDeviceDisplayPlanePropertiesKHR; otherwise pname:planeStackIndex must: equal the pname:currentStackIndex member of sname:VkDisplayPlanePropertiesKHR returned by fname:vkGetPhysicalDeviceDisplayPlanePropertiesKHR for the display plane corresponding to pname:displayMode
-- * If pname:alphaMode is ename:VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR then pname:globalAlpha must: be between `0` and `1`, inclusive
-- * pname:alphaMode must: be `0` or one of the bits present in the pname:supportedAlpha member of sname:VkDisplayPlaneCapabilitiesKHR returned by fname:vkGetDisplayPlaneCapabilitiesKHR for the display plane corresponding to pname:displayMode
-- * The pname:width and pname:height members of pname:imageExtent must: be less than the pname:maxImageDimensions2D member of sname:VkPhysicalDeviceLimits
data VkDisplaySurfaceCreateInfoKHR = VkDisplaySurfaceCreateInfoKHR
  { vdscikhr_sType :: !VkStructureType
  -- ^ `sType` 
  , vdscikhr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdscikhr_flags :: !VkDisplaySurfaceCreateFlagsKHR
  -- ^ `flags`  Can be `nullPtr`.
  , vdscikhr_displayMode :: !VkDisplayModeKHR
  -- ^ `displayMode` 
  , vdscikhr_planeIndex :: !Word32
  -- ^ `planeIndex` 
  , vdscikhr_planeStackIndex :: !Word32
  -- ^ `planeStackIndex` 
  , vdscikhr_transform :: !VkSurfaceTransformFlagBitsKHR
  -- ^ `transform` 
  , vdscikhr_globalAlpha :: !Float
  -- ^ `globalAlpha` 
  , vdscikhr_alphaMode :: !VkDisplayPlaneAlphaFlagBitsKHR
  -- ^ `alphaMode` 
  , vdscikhr_imageExtent :: !VkExtent2D
  -- ^ `imageExtent` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDisplaySurfaceCreateInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 32 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    displayMode <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    planeIndex <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    planeStackIndex <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    transform <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 2))
    globalAlpha <- peek (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3))
    alphaMode <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 3))
    imageExtent <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4))
    return $ VkDisplaySurfaceCreateInfoKHR sType pNext flags displayMode planeIndex planeStackIndex transform globalAlpha alphaMode imageExtent
  poke ptr (VkDisplaySurfaceCreateInfoKHR sType pNext flags displayMode planeIndex planeStackIndex transform globalAlpha alphaMode imageExtent) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) displayMode
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) planeIndex
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) planeStackIndex
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 2)) transform
    poke (plusPtr ptr (20 + sizeOf (undefined :: CSize) * 3)) globalAlpha
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 3)) alphaMode
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4)) imageExtent


-- | Man not found. VkDisplayPresentInfoKHR
-- 
-- == Validaty
-- * pname:srcRect must: specify a rectangular region that is a subset of the image being presented
-- * pname:dstRect must: specify a rectangular region that is a subset of the pname:visibleRegion parameter of the display mode the swapchain being presented uses
-- * If the pname:persistentContent member of the sname:VkDisplayPropertiesKHR structure returned by fname:vkGetPhysicalDeviceDisplayPropertiesKHR for the display the present operation targets then pname:persistent must: be ename:VK_FALSE
data VkDisplayPresentInfoKHR = VkDisplayPresentInfoKHR
  { vdpikhr_sType :: !VkStructureType
  -- ^ `sType` 
  , vdpikhr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdpikhr_srcRect :: !VkRect2D
  -- ^ `srcRect` 
  , vdpikhr_dstRect :: !VkRect2D
  -- ^ `dstRect` 
  , vdpikhr_persistent :: !VkBool32
  -- ^ `persistent` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDisplayPresentInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 36 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    srcRect <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    dstRect <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    persistent <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 2))
    return $ VkDisplayPresentInfoKHR sType pNext srcRect dstRect persistent
  poke ptr (VkDisplayPresentInfoKHR sType pNext srcRect dstRect persistent) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) srcRect
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) dstRect
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 2)) persistent


-- | Man not found. VkSurfaceCapabilitiesKHR
-- 
data VkSurfaceCapabilitiesKHR = VkSurfaceCapabilitiesKHR
  { vsckhr_minImageCount :: !Word32
  -- ^ `minImageCount` 
  , vsckhr_maxImageCount :: !Word32
  -- ^ `maxImageCount` 
  , vsckhr_currentExtent :: !VkExtent2D
  -- ^ `currentExtent` 
  , vsckhr_minImageExtent :: !VkExtent2D
  -- ^ `minImageExtent` 
  , vsckhr_maxImageExtent :: !VkExtent2D
  -- ^ `maxImageExtent` 
  , vsckhr_maxImageArrayLayers :: !Word32
  -- ^ `maxImageArrayLayers` 
  , vsckhr_supportedTransforms :: !VkSurfaceTransformFlagsKHR
  -- ^ `supportedTransforms`  Can be `nullPtr`.
  , vsckhr_currentTransform :: !VkSurfaceTransformFlagBitsKHR
  -- ^ `currentTransform` 
  , vsckhr_supportedCompositeAlpha :: !VkCompositeAlphaFlagsKHR
  -- ^ `supportedCompositeAlpha`  Can be `nullPtr`.
  , vsckhr_supportedUsageFlags :: !VkImageUsageFlags
  -- ^ `supportedUsageFlags`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSurfaceCapabilitiesKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 48 + sizeOf (undefined :: CSize) * 1
  peek ptr = do
    minImageCount <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    maxImageCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0))
    currentExtent <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0))
    minImageExtent <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0))
    maxImageExtent <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0))
    maxImageArrayLayers <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0))
    supportedTransforms <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0))
    currentTransform <- peek (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0))
    supportedCompositeAlpha <- peek (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 1))
    supportedUsageFlags <- peek (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 1))
    return $ VkSurfaceCapabilitiesKHR minImageCount maxImageCount currentExtent minImageExtent maxImageExtent maxImageArrayLayers supportedTransforms currentTransform supportedCompositeAlpha supportedUsageFlags
  poke ptr (VkSurfaceCapabilitiesKHR minImageCount maxImageCount currentExtent minImageExtent maxImageExtent maxImageArrayLayers supportedTransforms currentTransform supportedCompositeAlpha supportedUsageFlags) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) minImageCount
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 0)) maxImageCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 0)) currentExtent
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 0)) minImageExtent
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 0)) maxImageExtent
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 0)) maxImageArrayLayers
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 0)) supportedTransforms
    poke (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 0)) currentTransform
    poke (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 1)) supportedCompositeAlpha
    poke (plusPtr ptr (44 + sizeOf (undefined :: CSize) * 1)) supportedUsageFlags


-- | Man not found. VkAndroidSurfaceCreateInfoKHR
-- 
-- == Validaty
-- * pname:window mustnot: be in a connected state
data VkAndroidSurfaceCreateInfoKHR = VkAndroidSurfaceCreateInfoKHR
  { vascikhr_sType :: !VkStructureType
  -- ^ `sType` 
  , vascikhr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vascikhr_flags :: !VkAndroidSurfaceCreateFlagsKHR
  -- ^ `flags`  Can be `nullPtr`.
  , vascikhr_window :: !(Ptr ANativeWindow)
  -- ^ `window` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkAndroidSurfaceCreateInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    window <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    return $ VkAndroidSurfaceCreateInfoKHR sType pNext flags window
  poke ptr (VkAndroidSurfaceCreateInfoKHR sType pNext flags window) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) window


-- | Man not found. VkMirSurfaceCreateInfoKHR
-- 
data VkMirSurfaceCreateInfoKHR = VkMirSurfaceCreateInfoKHR
  { vmscikhr_sType :: !VkStructureType
  -- ^ `sType` 
  , vmscikhr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vmscikhr_flags :: !VkMirSurfaceCreateFlagsKHR
  -- ^ `flags`  Can be `nullPtr`.
  , vmscikhr_connection :: !(Ptr MirConnection)
  -- ^ `connection` 
  , vmscikhr_mirSurface :: !(Ptr MirSurface)
  -- ^ `mirSurface` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkMirSurfaceCreateInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    connection <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    mirSurface <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    return $ VkMirSurfaceCreateInfoKHR sType pNext flags connection mirSurface
  poke ptr (VkMirSurfaceCreateInfoKHR sType pNext flags connection mirSurface) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) connection
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) mirSurface


-- | Man not found. VkWaylandSurfaceCreateInfoKHR
-- 
data VkWaylandSurfaceCreateInfoKHR = VkWaylandSurfaceCreateInfoKHR
  { vwscikhr_sType :: !VkStructureType
  -- ^ `sType` 
  , vwscikhr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vwscikhr_flags :: !VkWaylandSurfaceCreateFlagsKHR
  -- ^ `flags`  Can be `nullPtr`.
  , vwscikhr_display :: !(Ptr WlDisplay)
  -- ^ `display` 
  , vwscikhr_surface :: !(Ptr WlSurface)
  -- ^ `surface` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkWaylandSurfaceCreateInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    display <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    surface <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    return $ VkWaylandSurfaceCreateInfoKHR sType pNext flags display surface
  poke ptr (VkWaylandSurfaceCreateInfoKHR sType pNext flags display surface) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) display
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) surface


-- | Man not found. VkWin32SurfaceCreateInfoKHR
-- 
data VkWin32SurfaceCreateInfoKHR = VkWin32SurfaceCreateInfoKHR
  { vw32scikhr_sType :: !VkStructureType
  -- ^ `sType` 
  , vw32scikhr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vw32scikhr_flags :: !VkWin32SurfaceCreateFlagsKHR
  -- ^ `flags`  Can be `nullPtr`.
  , vw32scikhr_hinstance :: !HINSTANCE
  -- ^ `hinstance` 
  , vw32scikhr_hwnd :: !HWND
  -- ^ `hwnd` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkWin32SurfaceCreateInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    hinstance <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    hwnd <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    return $ VkWin32SurfaceCreateInfoKHR sType pNext flags hinstance hwnd
  poke ptr (VkWin32SurfaceCreateInfoKHR sType pNext flags hinstance hwnd) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) hinstance
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) hwnd


-- | Man not found. VkXlibSurfaceCreateInfoKHR
-- 
data VkXlibSurfaceCreateInfoKHR = VkXlibSurfaceCreateInfoKHR
  { vxlibsci_sType :: !VkStructureType
  -- ^ `sType` 
  , vxlibsci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vxlibsci_flags :: !VkXlibSurfaceCreateFlagsKHR
  -- ^ `flags`  Can be `nullPtr`.
  , vxlibsci_dpy :: !(Ptr Display)
  -- ^ `dpy` 
  , vxlibsci_window :: !Window
  -- ^ `window` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkXlibSurfaceCreateInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    dpy <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    window <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    return $ VkXlibSurfaceCreateInfoKHR sType pNext flags dpy window
  poke ptr (VkXlibSurfaceCreateInfoKHR sType pNext flags dpy window) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) dpy
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) window


-- | Man not found. VkXcbSurfaceCreateInfoKHR
-- 
data VkXcbSurfaceCreateInfoKHR = VkXcbSurfaceCreateInfoKHR
  { vxcbsci_sType :: !VkStructureType
  -- ^ `sType` 
  , vxcbsci_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vxcbsci_flags :: !VkXcbSurfaceCreateFlagsKHR
  -- ^ `flags`  Can be `nullPtr`.
  , vxcbsci_connection :: !(Ptr XcbConnection)
  -- ^ `connection` 
  , vxcbsci_window :: !XcbWindow
  -- ^ `window` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkXcbSurfaceCreateInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    connection <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    window <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    return $ VkXcbSurfaceCreateInfoKHR sType pNext flags connection window
  poke ptr (VkXcbSurfaceCreateInfoKHR sType pNext flags connection window) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) connection
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) window


-- | Man not found. VkSurfaceFormatKHR
-- 
data VkSurfaceFormatKHR = VkSurfaceFormatKHR
  { vsfkhr_format :: !VkFormat
  -- ^ `format` 
  , vsfkhr_colorSpace :: !VkColorSpaceKHR
  -- ^ `colorSpace` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSurfaceFormatKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 0 + sizeOf (undefined :: CSize) * 2
  peek ptr = do
    format <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    colorSpace <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    return $ VkSurfaceFormatKHR format colorSpace
  poke ptr (VkSurfaceFormatKHR format colorSpace) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) format
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) colorSpace


-- | Man not found. VkSwapchainCreateInfoKHR
-- 
-- == Validaty
-- * pname:surface must: be a surface that is supported by the device as determined using fname:vkGetPhysicalDeviceSurfaceSupportKHR
-- * The native window referred to by pname:surface mustnot: already be associated with a swapchain other than pname:oldSwapchain, or with a non-{apiname} graphics API surface
-- * pname:minImageCount must: be greater than or equal to the value returned in the pname:minImageCount member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
-- * pname:minImageCount must: be less than or equal to the value returned in the pname:maxImageCount member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface if the returned pname:maxImageCount is not zero
-- * pname:imageFormat and pname:imageColorspace must: match the pname:format and pname:colorSpace members, respectively, of one of the sname:VkSurfaceFormatKHR structures returned by fname:vkGetPhysicalDeviceSurfaceFormatsKHR for the surface
-- * pname:imageExtent must: be between pname:minImageExtent and pname:maxImageExtent, inclusive, where pname:minImageExtent and pname:maxImageExtent are members of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
-- * pname:imageArrayLayers must: be greater than `0` and less than or equal to the pname:maxImageArrayLayers member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
-- * pname:imageUsage must: be a subset of the supported usage flags present in the pname:supportedUsageFlags member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
-- * If pname:imageSharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:pQueueFamilyIndices must: be a pointer to an array of pname:queueFamilyIndexCount basetype:uint32_t values
-- * If pname:imageSharingMode is ename:VK_SHARING_MODE_CONCURRENT, pname:queueFamilyIndexCount must: be greater than `1`
-- * pname:preTransform must: be one of the bits present in the pname:supportedTransforms member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
-- * pname:compositeAlpha must: be one of the bits present in the pname:supportedCompositeAlpha member of the sname:VkSurfaceCapabilitiesKHR structure returned by fname:vkGetPhysicalDeviceSurfaceCapabilitiesKHR for the surface
-- * pname:presentMode must: be one of the ename:VkPresentModeKHR values returned by fname:vkGetPhysicalDeviceSurfacePresentModesKHR for the surface
data VkSwapchainCreateInfoKHR = VkSwapchainCreateInfoKHR
  { vscikhr_sType :: !VkStructureType
  -- ^ `sType` 
  , vscikhr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vscikhr_flags :: !VkSwapchainCreateFlagsKHR
  -- ^ `flags`  Can be `nullPtr`.
  , vscikhr_surface :: !VkSurfaceKHR
  -- ^ `surface` 
  , vscikhr_minImageCount :: !Word32
  -- ^ `minImageCount` 
  , vscikhr_imageFormat :: !VkFormat
  -- ^ `imageFormat` 
  , vscikhr_imageColorSpace :: !VkColorSpaceKHR
  -- ^ `imageColorSpace` 
  , vscikhr_imageExtent :: !VkExtent2D
  -- ^ `imageExtent` 
  , vscikhr_imageArrayLayers :: !Word32
  -- ^ `imageArrayLayers` 
  , vscikhr_imageUsage :: !VkImageUsageFlags
  -- ^ `imageUsage` 
  , vscikhr_imageSharingMode :: !VkSharingMode
  -- ^ `imageSharingMode` 
  , vscikhr_queueFamilyIndexCount :: !Word32
  -- ^ `queueFamilyIndexCount`  Can be `nullPtr`.
  , vscikhr_pQueueFamilyIndices :: !(Ptr Word32)
  -- ^ `pQueueFamilyIndices`  Const. Length: queueFamilyIndexCount. Noautovalidity.
  , vscikhr_preTransform :: !VkSurfaceTransformFlagBitsKHR
  -- ^ `preTransform` 
  , vscikhr_compositeAlpha :: !VkCompositeAlphaFlagBitsKHR
  -- ^ `compositeAlpha` 
  , vscikhr_presentMode :: !VkPresentModeKHR
  -- ^ `presentMode` 
  , vscikhr_clipped :: !VkBool32
  -- ^ `clipped` 
  , vscikhr_oldSwapchain :: !VkSwapchainKHR
  -- ^ `oldSwapchain`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkSwapchainCreateInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 48 + sizeOf (undefined :: CSize) * 9
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    surface <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    minImageCount <- peek (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2))
    imageFormat <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2))
    imageColorSpace <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3))
    imageExtent <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4))
    imageArrayLayers <- peek (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4))
    imageUsage <- peek (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 4))
    imageSharingMode <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 4))
    queueFamilyIndexCount <- peek (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 5))
    pQueueFamilyIndices <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 5))
    preTransform <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 6))
    compositeAlpha <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 7))
    presentMode <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 8))
    clipped <- peek (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 9))
    oldSwapchain <- peek (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 9))
    return $ VkSwapchainCreateInfoKHR sType pNext flags surface minImageCount imageFormat imageColorSpace imageExtent imageArrayLayers imageUsage imageSharingMode queueFamilyIndexCount pQueueFamilyIndices preTransform compositeAlpha presentMode clipped oldSwapchain
  poke ptr (VkSwapchainCreateInfoKHR sType pNext flags surface minImageCount imageFormat imageColorSpace imageExtent imageArrayLayers imageUsage imageSharingMode queueFamilyIndexCount pQueueFamilyIndices preTransform compositeAlpha presentMode clipped oldSwapchain) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) surface
    poke (plusPtr ptr (12 + sizeOf (undefined :: CSize) * 2)) minImageCount
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 2)) imageFormat
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3)) imageColorSpace
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4)) imageExtent
    poke (plusPtr ptr (24 + sizeOf (undefined :: CSize) * 4)) imageArrayLayers
    poke (plusPtr ptr (28 + sizeOf (undefined :: CSize) * 4)) imageUsage
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 4)) imageSharingMode
    poke (plusPtr ptr (32 + sizeOf (undefined :: CSize) * 5)) queueFamilyIndexCount
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 5)) pQueueFamilyIndices
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 6)) preTransform
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 7)) compositeAlpha
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 8)) presentMode
    poke (plusPtr ptr (36 + sizeOf (undefined :: CSize) * 9)) clipped
    poke (plusPtr ptr (40 + sizeOf (undefined :: CSize) * 9)) oldSwapchain


-- | Man not found. VkPresentInfoKHR
-- 
-- == Validaty
-- * Any given element of pname:pImageIndices must: be the index of a presentable image acquired from the swapchain specified by the corresponding element of the pname:pSwapchains array
-- * Any given element of sname:VkSemaphore in pname:pWaitSemaphores must: refer to a prior signal of that sname:VkSemaphore that won't be consumed by any other wait on that semaphore
data VkPresentInfoKHR = VkPresentInfoKHR
  { vpikhr_sType :: !VkStructureType
  -- ^ `sType` 
  , vpikhr_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vpikhr_waitSemaphoreCount :: !Word32
  -- ^ `waitSemaphoreCount`  Can be `nullPtr`.
  , vpikhr_pWaitSemaphores :: !(Ptr VkSemaphore)
  -- ^ `pWaitSemaphores`  Can be `nullPtr`. Const. Length: waitSemaphoreCount.
  , vpikhr_swapchainCount :: !Word32
  -- ^ `swapchainCount` 
  , vpikhr_pSwapchains :: !(Ptr VkSwapchainKHR)
  -- ^ `pSwapchains`  Const. Length: swapchainCount.
  , vpikhr_pImageIndices :: !(Ptr Word32)
  -- ^ `pImageIndices`  Const. Length: swapchainCount.
  , vpikhr_pResults :: !(Ptr VkResult)
  -- ^ `pResults`  Can be `nullPtr`. Length: swapchainCount.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPresentInfoKHR where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 6
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    waitSemaphoreCount <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    pWaitSemaphores <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    swapchainCount <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    pSwapchains <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    pImageIndices <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4))
    pResults <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 5))
    return $ VkPresentInfoKHR sType pNext waitSemaphoreCount pWaitSemaphores swapchainCount pSwapchains pImageIndices pResults
  poke ptr (VkPresentInfoKHR sType pNext waitSemaphoreCount pWaitSemaphores swapchainCount pSwapchains pImageIndices pResults) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) waitSemaphoreCount
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) pWaitSemaphores
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) swapchainCount
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) pSwapchains
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 4)) pImageIndices
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 5)) pResults


-- | Man not found. VkDebugReportCallbackCreateInfoEXT
-- 
data VkDebugReportCallbackCreateInfoEXT = VkDebugReportCallbackCreateInfoEXT
  { vdrcciext_sType :: !VkStructureType
  -- ^ `sType` 
  , vdrcciext_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdrcciext_flags :: !VkDebugReportFlagsEXT
  -- ^ `flags` 
  , vdrcciext_pfnCallback :: !PFN_vkDebugReportCallbackEXT
  -- ^ `pfnCallback` 
  , vdrcciext_pUserData :: !(Ptr ())
  -- ^ `pUserData`  Can be `nullPtr`.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDebugReportCallbackCreateInfoEXT where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 4 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    flags <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    pfnCallback <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2))
    pUserData <- peek (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3))
    return $ VkDebugReportCallbackCreateInfoEXT sType pNext flags pfnCallback pUserData
  poke ptr (VkDebugReportCallbackCreateInfoEXT sType pNext flags pfnCallback pUserData) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) flags
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 2)) pfnCallback
    poke (plusPtr ptr (4 + sizeOf (undefined :: CSize) * 3)) pUserData


-- | Man not found. VkPipelineRasterizationStateRasterizationOrderAMD
-- 
data VkPipelineRasterizationStateRasterizationOrderAMD = VkPipelineRasterizationStateRasterizationOrderAMD
  { vprsroamd_sType :: !VkStructureType
  -- ^ `sType` 
  , vprsroamd_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vprsroamd_rasterizationOrder :: !VkRasterizationOrderAMD
  -- ^ `rasterizationOrder` 
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkPipelineRasterizationStateRasterizationOrderAMD where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 0 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    rasterizationOrder <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    return $ VkPipelineRasterizationStateRasterizationOrderAMD sType pNext rasterizationOrder
  poke ptr (VkPipelineRasterizationStateRasterizationOrderAMD sType pNext rasterizationOrder) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) rasterizationOrder


-- | Man not found. VkDebugMarkerObjectNameInfoEXT
-- 
data VkDebugMarkerObjectNameInfoEXT = VkDebugMarkerObjectNameInfoEXT
  { vdmoniext_sType :: !VkStructureType
  -- ^ `sType` 
  , vdmoniext_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdmoniext_objectType :: !VkDebugReportObjectTypeEXT
  -- ^ `objectType` 
  , vdmoniext_object :: !Word64
  -- ^ `object` 
  , vdmoniext_pObjectName :: !(Ptr CChar)
  -- ^ `pObjectName`  Const. Length: null-terminated.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDebugMarkerObjectNameInfoEXT where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 8 + sizeOf (undefined :: CSize) * 4
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    objectType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    object <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3))
    pObjectName <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    return $ VkDebugMarkerObjectNameInfoEXT sType pNext objectType object pObjectName
  poke ptr (VkDebugMarkerObjectNameInfoEXT sType pNext objectType object pObjectName) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) objectType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3)) object
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) pObjectName


-- | Man not found. VkDebugMarkerObjectTagInfoEXT
-- 
data VkDebugMarkerObjectTagInfoEXT = VkDebugMarkerObjectTagInfoEXT
  { vdmotiext_sType :: !VkStructureType
  -- ^ `sType` 
  , vdmotiext_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdmotiext_objectType :: !VkDebugReportObjectTypeEXT
  -- ^ `objectType` 
  , vdmotiext_object :: !Word64
  -- ^ `object` 
  , vdmotiext_tagName :: !Word64
  -- ^ `tagName` 
  , vdmotiext_tagSize :: !CSize
  -- ^ `tagSize` 
  , vdmotiext_pTag :: !(Ptr ())
  -- ^ `pTag`  Const. Length: tagSize.
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDebugMarkerObjectTagInfoEXT where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 5
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    objectType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    object <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3))
    tagName <- peek (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3))
    tagSize <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3))
    pTag <- peek (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4))
    return $ VkDebugMarkerObjectTagInfoEXT sType pNext objectType object tagName tagSize pTag
  poke ptr (VkDebugMarkerObjectTagInfoEXT sType pNext objectType object tagName tagSize pTag) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) objectType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3)) object
    poke (plusPtr ptr (8 + sizeOf (undefined :: CSize) * 3)) tagName
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 3)) tagSize
    poke (plusPtr ptr (16 + sizeOf (undefined :: CSize) * 4)) pTag


-- | Man not found. VkDebugMarkerMarkerInfoEXT
-- 
data VkDebugMarkerMarkerInfoEXT = VkDebugMarkerMarkerInfoEXT
  { vdmmiext_sType :: !VkStructureType
  -- ^ `sType` 
  , vdmmiext_pNext :: !(Ptr ())
  -- ^ `pNext`  Const.
  , vdmmiext_pMarkerName :: !(Ptr CChar)
  -- ^ `pMarkerName`  Const. Length: null-terminated.
  , vdmmiext_color :: !(V4 Float)
  -- ^ `color`  Can be `nullPtr`. Max: [4].
  } --deriving (Eq, Show)

-- ReturnedOnly = False
instance Storable VkDebugMarkerMarkerInfoEXT where
  alignment _ = alignment (undefined :: CSize)
  sizeOf _ = 16 + sizeOf (undefined :: CSize) * 3
  peek ptr = do
    sType <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0))
    pNext <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1))
    pMarkerName <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2))
    color <- peek (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3))
    return $ VkDebugMarkerMarkerInfoEXT sType pNext pMarkerName color
  poke ptr (VkDebugMarkerMarkerInfoEXT sType pNext pMarkerName color) = do
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 0)) sType
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 1)) pNext
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 2)) pMarkerName
    poke (plusPtr ptr (0 + sizeOf (undefined :: CSize) * 3)) color

-- | 
newtype VkImageLayout = VkImageLayout Int deriving (Eq, Ord, Show, Storable)

-- | Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0
-- | General layout when image can be used for any kind of access
pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1
-- | Optimal layout when image is only used for color attachment read/write
pattern VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL = VkImageLayout 2
-- | Optimal layout when image is only used for depth/stencil attachment read/write
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = VkImageLayout 3
-- | Optimal layout when image is used for read only depth/stencil attachment and shader access
pattern VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL = VkImageLayout 4
-- | Optimal layout when image is used for read only shader access
pattern VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL = VkImageLayout 5
-- | Optimal layout when image is used only as source of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL = VkImageLayout 6
-- | Optimal layout when image is used only as destination of transfer operations
pattern VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL = VkImageLayout 7
-- | Initial layout used when the data is populated by the CPU
pattern VK_IMAGE_LAYOUT_PREINITIALIZED = VkImageLayout 8


-- | 
newtype VkAttachmentLoadOp = VkAttachmentLoadOp Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0
-- | 
pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1
-- | 
pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2


-- | 
newtype VkAttachmentStoreOp = VkAttachmentStoreOp Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0
-- | 
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1


-- | 
newtype VkImageType = VkImageType Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_IMAGE_TYPE_1D = VkImageType 0
-- | 
pattern VK_IMAGE_TYPE_2D = VkImageType 1
-- | 
pattern VK_IMAGE_TYPE_3D = VkImageType 2


-- | 
newtype VkImageTiling = VkImageTiling Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0
-- | 
pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1


-- | 
newtype VkImageViewType = VkImageViewType Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_IMAGE_VIEW_TYPE_1D = VkImageViewType 0
-- | 
pattern VK_IMAGE_VIEW_TYPE_2D = VkImageViewType 1
-- | 
pattern VK_IMAGE_VIEW_TYPE_3D = VkImageViewType 2
-- | 
pattern VK_IMAGE_VIEW_TYPE_CUBE = VkImageViewType 3
-- | 
pattern VK_IMAGE_VIEW_TYPE_1D_ARRAY = VkImageViewType 4
-- | 
pattern VK_IMAGE_VIEW_TYPE_2D_ARRAY = VkImageViewType 5
-- | 
pattern VK_IMAGE_VIEW_TYPE_CUBE_ARRAY = VkImageViewType 6


-- | 
newtype VkCommandBufferLevel = VkCommandBufferLevel Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_COMMAND_BUFFER_LEVEL_PRIMARY = VkCommandBufferLevel 0
-- | 
pattern VK_COMMAND_BUFFER_LEVEL_SECONDARY = VkCommandBufferLevel 1


-- | 
newtype VkComponentSwizzle = VkComponentSwizzle Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_COMPONENT_SWIZZLE_IDENTITY = VkComponentSwizzle 0
-- | 
pattern VK_COMPONENT_SWIZZLE_ZERO = VkComponentSwizzle 1
-- | 
pattern VK_COMPONENT_SWIZZLE_ONE = VkComponentSwizzle 2
-- | 
pattern VK_COMPONENT_SWIZZLE_R = VkComponentSwizzle 3
-- | 
pattern VK_COMPONENT_SWIZZLE_G = VkComponentSwizzle 4
-- | 
pattern VK_COMPONENT_SWIZZLE_B = VkComponentSwizzle 5
-- | 
pattern VK_COMPONENT_SWIZZLE_A = VkComponentSwizzle 6


-- | 
newtype VkDescriptorType = VkDescriptorType Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_DESCRIPTOR_TYPE_SAMPLER = VkDescriptorType 0
-- | 
pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = VkDescriptorType 1
-- | 
pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = VkDescriptorType 2
-- | 
pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = VkDescriptorType 3
-- | 
pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = VkDescriptorType 4
-- | 
pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = VkDescriptorType 5
-- | 
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = VkDescriptorType 6
-- | 
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = VkDescriptorType 7
-- | 
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = VkDescriptorType 8
-- | 
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = VkDescriptorType 9
-- | 
pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = VkDescriptorType 10


-- | 
newtype VkQueryType = VkQueryType Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0
-- | Optional
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1
-- | 
pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2


-- | 
newtype VkBorderColor = VkBorderColor Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = VkBorderColor 0
-- | 
pattern VK_BORDER_COLOR_INT_TRANSPARENT_BLACK = VkBorderColor 1
-- | 
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK = VkBorderColor 2
-- | 
pattern VK_BORDER_COLOR_INT_OPAQUE_BLACK = VkBorderColor 3
-- | 
pattern VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE = VkBorderColor 4
-- | 
pattern VK_BORDER_COLOR_INT_OPAQUE_WHITE = VkBorderColor 5


-- | 
newtype VkPipelineBindPoint = VkPipelineBindPoint Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0
-- | 
pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1


-- | 
newtype VkPipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE = VkPipelineCacheHeaderVersion 1


-- | 
newtype VkPrimitiveTopology = VkPrimitiveTopology Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST = VkPrimitiveTopology 0
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST = VkPrimitiveTopology 1
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = VkPrimitiveTopology 2
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = VkPrimitiveTopology 3
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = VkPrimitiveTopology 4
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = VkPrimitiveTopology 5
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY = VkPrimitiveTopology 6
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY = VkPrimitiveTopology 7
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY = VkPrimitiveTopology 8
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = VkPrimitiveTopology 9
-- | 
pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST = VkPrimitiveTopology 10


-- | 
newtype VkSharingMode = VkSharingMode Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_SHARING_MODE_EXCLUSIVE = VkSharingMode 0
-- | 
pattern VK_SHARING_MODE_CONCURRENT = VkSharingMode 1


-- | 
newtype VkIndexType = VkIndexType Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_INDEX_TYPE_UINT16 = VkIndexType 0
-- | 
pattern VK_INDEX_TYPE_UINT32 = VkIndexType 1


-- | 
newtype VkFilter = VkFilter Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_FILTER_NEAREST = VkFilter 0
-- | 
pattern VK_FILTER_LINEAR = VkFilter 1


-- | 
newtype VkSamplerMipmapMode = VkSamplerMipmapMode Int deriving (Eq, Ord, Show, Storable)

-- | Choose nearest mip level
pattern VK_SAMPLER_MIPMAP_MODE_NEAREST = VkSamplerMipmapMode 0
-- | Linear filter between mip levels
pattern VK_SAMPLER_MIPMAP_MODE_LINEAR = VkSamplerMipmapMode 1


-- | 
newtype VkSamplerAddressMode = VkSamplerAddressMode Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_SAMPLER_ADDRESS_MODE_REPEAT = VkSamplerAddressMode 0
-- | 
pattern VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = VkSamplerAddressMode 1
-- | 
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = VkSamplerAddressMode 2
-- | 
pattern VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = VkSamplerAddressMode 3


-- | 
newtype VkCompareOp = VkCompareOp Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_COMPARE_OP_NEVER = VkCompareOp 0
-- | 
pattern VK_COMPARE_OP_LESS = VkCompareOp 1
-- | 
pattern VK_COMPARE_OP_EQUAL = VkCompareOp 2
-- | 
pattern VK_COMPARE_OP_LESS_OR_EQUAL = VkCompareOp 3
-- | 
pattern VK_COMPARE_OP_GREATER = VkCompareOp 4
-- | 
pattern VK_COMPARE_OP_NOT_EQUAL = VkCompareOp 5
-- | 
pattern VK_COMPARE_OP_GREATER_OR_EQUAL = VkCompareOp 6
-- | 
pattern VK_COMPARE_OP_ALWAYS = VkCompareOp 7


-- | 
newtype VkPolygonMode = VkPolygonMode Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_POLYGON_MODE_FILL = VkPolygonMode 0
-- | 
pattern VK_POLYGON_MODE_LINE = VkPolygonMode 1
-- | 
pattern VK_POLYGON_MODE_POINT = VkPolygonMode 2


-- | 
newtype VkCullModeFlagBits = VkCullModeFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_CULL_MODE_NONE = VkCullModeFlagBits 0
-- | 
pattern VK_CULL_MODE_FRONT_BIT = VkCullModeFlagBits 1
-- | 
pattern VK_CULL_MODE_BACK_BIT = VkCullModeFlagBits 2
-- | 
pattern VK_CULL_MODE_FRONT_AND_BACK = VkCullModeFlagBits 0x00000003


-- | 
newtype VkFrontFace = VkFrontFace Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_FRONT_FACE_COUNTER_CLOCKWISE = VkFrontFace 0
-- | 
pattern VK_FRONT_FACE_CLOCKWISE = VkFrontFace 1


-- | 
newtype VkBlendFactor = VkBlendFactor Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_BLEND_FACTOR_ZERO = VkBlendFactor 0
-- | 
pattern VK_BLEND_FACTOR_ONE = VkBlendFactor 1
-- | 
pattern VK_BLEND_FACTOR_SRC_COLOR = VkBlendFactor 2
-- | 
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = VkBlendFactor 3
-- | 
pattern VK_BLEND_FACTOR_DST_COLOR = VkBlendFactor 4
-- | 
pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = VkBlendFactor 5
-- | 
pattern VK_BLEND_FACTOR_SRC_ALPHA = VkBlendFactor 6
-- | 
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = VkBlendFactor 7
-- | 
pattern VK_BLEND_FACTOR_DST_ALPHA = VkBlendFactor 8
-- | 
pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = VkBlendFactor 9
-- | 
pattern VK_BLEND_FACTOR_CONSTANT_COLOR = VkBlendFactor 10
-- | 
pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = VkBlendFactor 11
-- | 
pattern VK_BLEND_FACTOR_CONSTANT_ALPHA = VkBlendFactor 12
-- | 
pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = VkBlendFactor 13
-- | 
pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE = VkBlendFactor 14
-- | 
pattern VK_BLEND_FACTOR_SRC1_COLOR = VkBlendFactor 15
-- | 
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = VkBlendFactor 16
-- | 
pattern VK_BLEND_FACTOR_SRC1_ALPHA = VkBlendFactor 17
-- | 
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = VkBlendFactor 18


-- | 
newtype VkBlendOp = VkBlendOp Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_BLEND_OP_ADD = VkBlendOp 0
-- | 
pattern VK_BLEND_OP_SUBTRACT = VkBlendOp 1
-- | 
pattern VK_BLEND_OP_REVERSE_SUBTRACT = VkBlendOp 2
-- | 
pattern VK_BLEND_OP_MIN = VkBlendOp 3
-- | 
pattern VK_BLEND_OP_MAX = VkBlendOp 4


-- | 
newtype VkStencilOp = VkStencilOp Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_STENCIL_OP_KEEP = VkStencilOp 0
-- | 
pattern VK_STENCIL_OP_ZERO = VkStencilOp 1
-- | 
pattern VK_STENCIL_OP_REPLACE = VkStencilOp 2
-- | 
pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP = VkStencilOp 3
-- | 
pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP = VkStencilOp 4
-- | 
pattern VK_STENCIL_OP_INVERT = VkStencilOp 5
-- | 
pattern VK_STENCIL_OP_INCREMENT_AND_WRAP = VkStencilOp 6
-- | 
pattern VK_STENCIL_OP_DECREMENT_AND_WRAP = VkStencilOp 7


-- | 
newtype VkLogicOp = VkLogicOp Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_LOGIC_OP_CLEAR = VkLogicOp 0
-- | 
pattern VK_LOGIC_OP_AND = VkLogicOp 1
-- | 
pattern VK_LOGIC_OP_AND_REVERSE = VkLogicOp 2
-- | 
pattern VK_LOGIC_OP_COPY = VkLogicOp 3
-- | 
pattern VK_LOGIC_OP_AND_INVERTED = VkLogicOp 4
-- | 
pattern VK_LOGIC_OP_NO_OP = VkLogicOp 5
-- | 
pattern VK_LOGIC_OP_XOR = VkLogicOp 6
-- | 
pattern VK_LOGIC_OP_OR = VkLogicOp 7
-- | 
pattern VK_LOGIC_OP_NOR = VkLogicOp 8
-- | 
pattern VK_LOGIC_OP_EQUIVALENT = VkLogicOp 9
-- | 
pattern VK_LOGIC_OP_INVERT = VkLogicOp 10
-- | 
pattern VK_LOGIC_OP_OR_REVERSE = VkLogicOp 11
-- | 
pattern VK_LOGIC_OP_COPY_INVERTED = VkLogicOp 12
-- | 
pattern VK_LOGIC_OP_OR_INVERTED = VkLogicOp 13
-- | 
pattern VK_LOGIC_OP_NAND = VkLogicOp 14
-- | 
pattern VK_LOGIC_OP_SET = VkLogicOp 15


-- | 
newtype VkInternalAllocationType = VkInternalAllocationType Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = VkInternalAllocationType 0


-- | 
newtype VkSystemAllocationScope = VkSystemAllocationScope Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = VkSystemAllocationScope 0
-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = VkSystemAllocationScope 1
-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE = VkSystemAllocationScope 2
-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = VkSystemAllocationScope 3
-- | 
pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = VkSystemAllocationScope 4


-- | 
newtype VkPhysicalDeviceType = VkPhysicalDeviceType Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_OTHER = VkPhysicalDeviceType 0
-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = VkPhysicalDeviceType 1
-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = VkPhysicalDeviceType 2
-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = VkPhysicalDeviceType 3
-- | 
pattern VK_PHYSICAL_DEVICE_TYPE_CPU = VkPhysicalDeviceType 4


-- | 
newtype VkVertexInputRate = VkVertexInputRate Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_VERTEX_INPUT_RATE_VERTEX = VkVertexInputRate 0
-- | 
pattern VK_VERTEX_INPUT_RATE_INSTANCE = VkVertexInputRate 1


-- | Vulkan format definitions
newtype VkFormat = VkFormat Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_FORMAT_UNDEFINED = VkFormat 0
-- | 
pattern VK_FORMAT_R4G4_UNORM_PACK8 = VkFormat 1
-- | 
pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 = VkFormat 2
-- | 
pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 = VkFormat 3
-- | 
pattern VK_FORMAT_R5G6B5_UNORM_PACK16 = VkFormat 4
-- | 
pattern VK_FORMAT_B5G6R5_UNORM_PACK16 = VkFormat 5
-- | 
pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 = VkFormat 6
-- | 
pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 = VkFormat 7
-- | 
pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 = VkFormat 8
-- | 
pattern VK_FORMAT_R8_UNORM = VkFormat 9
-- | 
pattern VK_FORMAT_R8_SNORM = VkFormat 10
-- | 
pattern VK_FORMAT_R8_USCALED = VkFormat 11
-- | 
pattern VK_FORMAT_R8_SSCALED = VkFormat 12
-- | 
pattern VK_FORMAT_R8_UINT = VkFormat 13
-- | 
pattern VK_FORMAT_R8_SINT = VkFormat 14
-- | 
pattern VK_FORMAT_R8_SRGB = VkFormat 15
-- | 
pattern VK_FORMAT_R8G8_UNORM = VkFormat 16
-- | 
pattern VK_FORMAT_R8G8_SNORM = VkFormat 17
-- | 
pattern VK_FORMAT_R8G8_USCALED = VkFormat 18
-- | 
pattern VK_FORMAT_R8G8_SSCALED = VkFormat 19
-- | 
pattern VK_FORMAT_R8G8_UINT = VkFormat 20
-- | 
pattern VK_FORMAT_R8G8_SINT = VkFormat 21
-- | 
pattern VK_FORMAT_R8G8_SRGB = VkFormat 22
-- | 
pattern VK_FORMAT_R8G8B8_UNORM = VkFormat 23
-- | 
pattern VK_FORMAT_R8G8B8_SNORM = VkFormat 24
-- | 
pattern VK_FORMAT_R8G8B8_USCALED = VkFormat 25
-- | 
pattern VK_FORMAT_R8G8B8_SSCALED = VkFormat 26
-- | 
pattern VK_FORMAT_R8G8B8_UINT = VkFormat 27
-- | 
pattern VK_FORMAT_R8G8B8_SINT = VkFormat 28
-- | 
pattern VK_FORMAT_R8G8B8_SRGB = VkFormat 29
-- | 
pattern VK_FORMAT_B8G8R8_UNORM = VkFormat 30
-- | 
pattern VK_FORMAT_B8G8R8_SNORM = VkFormat 31
-- | 
pattern VK_FORMAT_B8G8R8_USCALED = VkFormat 32
-- | 
pattern VK_FORMAT_B8G8R8_SSCALED = VkFormat 33
-- | 
pattern VK_FORMAT_B8G8R8_UINT = VkFormat 34
-- | 
pattern VK_FORMAT_B8G8R8_SINT = VkFormat 35
-- | 
pattern VK_FORMAT_B8G8R8_SRGB = VkFormat 36
-- | 
pattern VK_FORMAT_R8G8B8A8_UNORM = VkFormat 37
-- | 
pattern VK_FORMAT_R8G8B8A8_SNORM = VkFormat 38
-- | 
pattern VK_FORMAT_R8G8B8A8_USCALED = VkFormat 39
-- | 
pattern VK_FORMAT_R8G8B8A8_SSCALED = VkFormat 40
-- | 
pattern VK_FORMAT_R8G8B8A8_UINT = VkFormat 41
-- | 
pattern VK_FORMAT_R8G8B8A8_SINT = VkFormat 42
-- | 
pattern VK_FORMAT_R8G8B8A8_SRGB = VkFormat 43
-- | 
pattern VK_FORMAT_B8G8R8A8_UNORM = VkFormat 44
-- | 
pattern VK_FORMAT_B8G8R8A8_SNORM = VkFormat 45
-- | 
pattern VK_FORMAT_B8G8R8A8_USCALED = VkFormat 46
-- | 
pattern VK_FORMAT_B8G8R8A8_SSCALED = VkFormat 47
-- | 
pattern VK_FORMAT_B8G8R8A8_UINT = VkFormat 48
-- | 
pattern VK_FORMAT_B8G8R8A8_SINT = VkFormat 49
-- | 
pattern VK_FORMAT_B8G8R8A8_SRGB = VkFormat 50
-- | 
pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 = VkFormat 51
-- | 
pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 = VkFormat 52
-- | 
pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 = VkFormat 53
-- | 
pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = VkFormat 54
-- | 
pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 = VkFormat 55
-- | 
pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 = VkFormat 56
-- | 
pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 = VkFormat 57
-- | 
pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 = VkFormat 58
-- | 
pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 = VkFormat 59
-- | 
pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 = VkFormat 60
-- | 
pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = VkFormat 61
-- | 
pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 = VkFormat 62
-- | 
pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 = VkFormat 63
-- | 
pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 = VkFormat 64
-- | 
pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 = VkFormat 65
-- | 
pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 = VkFormat 66
-- | 
pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = VkFormat 67
-- | 
pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 = VkFormat 68
-- | 
pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 = VkFormat 69
-- | 
pattern VK_FORMAT_R16_UNORM = VkFormat 70
-- | 
pattern VK_FORMAT_R16_SNORM = VkFormat 71
-- | 
pattern VK_FORMAT_R16_USCALED = VkFormat 72
-- | 
pattern VK_FORMAT_R16_SSCALED = VkFormat 73
-- | 
pattern VK_FORMAT_R16_UINT = VkFormat 74
-- | 
pattern VK_FORMAT_R16_SINT = VkFormat 75
-- | 
pattern VK_FORMAT_R16_SFLOAT = VkFormat 76
-- | 
pattern VK_FORMAT_R16G16_UNORM = VkFormat 77
-- | 
pattern VK_FORMAT_R16G16_SNORM = VkFormat 78
-- | 
pattern VK_FORMAT_R16G16_USCALED = VkFormat 79
-- | 
pattern VK_FORMAT_R16G16_SSCALED = VkFormat 80
-- | 
pattern VK_FORMAT_R16G16_UINT = VkFormat 81
-- | 
pattern VK_FORMAT_R16G16_SINT = VkFormat 82
-- | 
pattern VK_FORMAT_R16G16_SFLOAT = VkFormat 83
-- | 
pattern VK_FORMAT_R16G16B16_UNORM = VkFormat 84
-- | 
pattern VK_FORMAT_R16G16B16_SNORM = VkFormat 85
-- | 
pattern VK_FORMAT_R16G16B16_USCALED = VkFormat 86
-- | 
pattern VK_FORMAT_R16G16B16_SSCALED = VkFormat 87
-- | 
pattern VK_FORMAT_R16G16B16_UINT = VkFormat 88
-- | 
pattern VK_FORMAT_R16G16B16_SINT = VkFormat 89
-- | 
pattern VK_FORMAT_R16G16B16_SFLOAT = VkFormat 90
-- | 
pattern VK_FORMAT_R16G16B16A16_UNORM = VkFormat 91
-- | 
pattern VK_FORMAT_R16G16B16A16_SNORM = VkFormat 92
-- | 
pattern VK_FORMAT_R16G16B16A16_USCALED = VkFormat 93
-- | 
pattern VK_FORMAT_R16G16B16A16_SSCALED = VkFormat 94
-- | 
pattern VK_FORMAT_R16G16B16A16_UINT = VkFormat 95
-- | 
pattern VK_FORMAT_R16G16B16A16_SINT = VkFormat 96
-- | 
pattern VK_FORMAT_R16G16B16A16_SFLOAT = VkFormat 97
-- | 
pattern VK_FORMAT_R32_UINT = VkFormat 98
-- | 
pattern VK_FORMAT_R32_SINT = VkFormat 99
-- | 
pattern VK_FORMAT_R32_SFLOAT = VkFormat 100
-- | 
pattern VK_FORMAT_R32G32_UINT = VkFormat 101
-- | 
pattern VK_FORMAT_R32G32_SINT = VkFormat 102
-- | 
pattern VK_FORMAT_R32G32_SFLOAT = VkFormat 103
-- | 
pattern VK_FORMAT_R32G32B32_UINT = VkFormat 104
-- | 
pattern VK_FORMAT_R32G32B32_SINT = VkFormat 105
-- | 
pattern VK_FORMAT_R32G32B32_SFLOAT = VkFormat 106
-- | 
pattern VK_FORMAT_R32G32B32A32_UINT = VkFormat 107
-- | 
pattern VK_FORMAT_R32G32B32A32_SINT = VkFormat 108
-- | 
pattern VK_FORMAT_R32G32B32A32_SFLOAT = VkFormat 109
-- | 
pattern VK_FORMAT_R64_UINT = VkFormat 110
-- | 
pattern VK_FORMAT_R64_SINT = VkFormat 111
-- | 
pattern VK_FORMAT_R64_SFLOAT = VkFormat 112
-- | 
pattern VK_FORMAT_R64G64_UINT = VkFormat 113
-- | 
pattern VK_FORMAT_R64G64_SINT = VkFormat 114
-- | 
pattern VK_FORMAT_R64G64_SFLOAT = VkFormat 115
-- | 
pattern VK_FORMAT_R64G64B64_UINT = VkFormat 116
-- | 
pattern VK_FORMAT_R64G64B64_SINT = VkFormat 117
-- | 
pattern VK_FORMAT_R64G64B64_SFLOAT = VkFormat 118
-- | 
pattern VK_FORMAT_R64G64B64A64_UINT = VkFormat 119
-- | 
pattern VK_FORMAT_R64G64B64A64_SINT = VkFormat 120
-- | 
pattern VK_FORMAT_R64G64B64A64_SFLOAT = VkFormat 121
-- | 
pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 = VkFormat 122
-- | 
pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = VkFormat 123
-- | 
pattern VK_FORMAT_D16_UNORM = VkFormat 124
-- | 
pattern VK_FORMAT_X8_D24_UNORM_PACK32 = VkFormat 125
-- | 
pattern VK_FORMAT_D32_SFLOAT = VkFormat 126
-- | 
pattern VK_FORMAT_S8_UINT = VkFormat 127
-- | 
pattern VK_FORMAT_D16_UNORM_S8_UINT = VkFormat 128
-- | 
pattern VK_FORMAT_D24_UNORM_S8_UINT = VkFormat 129
-- | 
pattern VK_FORMAT_D32_SFLOAT_S8_UINT = VkFormat 130
-- | 
pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK = VkFormat 131
-- | 
pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK = VkFormat 132
-- | 
pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK = VkFormat 133
-- | 
pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK = VkFormat 134
-- | 
pattern VK_FORMAT_BC2_UNORM_BLOCK = VkFormat 135
-- | 
pattern VK_FORMAT_BC2_SRGB_BLOCK = VkFormat 136
-- | 
pattern VK_FORMAT_BC3_UNORM_BLOCK = VkFormat 137
-- | 
pattern VK_FORMAT_BC3_SRGB_BLOCK = VkFormat 138
-- | 
pattern VK_FORMAT_BC4_UNORM_BLOCK = VkFormat 139
-- | 
pattern VK_FORMAT_BC4_SNORM_BLOCK = VkFormat 140
-- | 
pattern VK_FORMAT_BC5_UNORM_BLOCK = VkFormat 141
-- | 
pattern VK_FORMAT_BC5_SNORM_BLOCK = VkFormat 142
-- | 
pattern VK_FORMAT_BC6H_UFLOAT_BLOCK = VkFormat 143
-- | 
pattern VK_FORMAT_BC6H_SFLOAT_BLOCK = VkFormat 144
-- | 
pattern VK_FORMAT_BC7_UNORM_BLOCK = VkFormat 145
-- | 
pattern VK_FORMAT_BC7_SRGB_BLOCK = VkFormat 146
-- | 
pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = VkFormat 147
-- | 
pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = VkFormat 148
-- | 
pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = VkFormat 149
-- | 
pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = VkFormat 150
-- | 
pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = VkFormat 151
-- | 
pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = VkFormat 152
-- | 
pattern VK_FORMAT_EAC_R11_UNORM_BLOCK = VkFormat 153
-- | 
pattern VK_FORMAT_EAC_R11_SNORM_BLOCK = VkFormat 154
-- | 
pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK = VkFormat 155
-- | 
pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK = VkFormat 156
-- | 
pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK = VkFormat 157
-- | 
pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK = VkFormat 158
-- | 
pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK = VkFormat 159
-- | 
pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK = VkFormat 160
-- | 
pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK = VkFormat 161
-- | 
pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK = VkFormat 162
-- | 
pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK = VkFormat 163
-- | 
pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK = VkFormat 164
-- | 
pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK = VkFormat 165
-- | 
pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK = VkFormat 166
-- | 
pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK = VkFormat 167
-- | 
pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK = VkFormat 168
-- | 
pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK = VkFormat 169
-- | 
pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK = VkFormat 170
-- | 
pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK = VkFormat 171
-- | 
pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK = VkFormat 172
-- | 
pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK = VkFormat 173
-- | 
pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK = VkFormat 174
-- | 
pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK = VkFormat 175
-- | 
pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK = VkFormat 176
-- | 
pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK = VkFormat 177
-- | 
pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK = VkFormat 178
-- | 
pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK = VkFormat 179
-- | 
pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK = VkFormat 180
-- | 
pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK = VkFormat 181
-- | 
pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK = VkFormat 182
-- | 
pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK = VkFormat 183
-- | 
pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK = VkFormat 184


-- | Structure type enumerant
newtype VkStructureType = VkStructureType Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_STRUCTURE_TYPE_APPLICATION_INFO = VkStructureType 0
-- | 
pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = VkStructureType 1
-- | 
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = VkStructureType 2
-- | 
pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = VkStructureType 3
-- | 
pattern VK_STRUCTURE_TYPE_SUBMIT_INFO = VkStructureType 4
-- | 
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = VkStructureType 5
-- | 
pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = VkStructureType 6
-- | 
pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO = VkStructureType 7
-- | 
pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = VkStructureType 8
-- | 
pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = VkStructureType 9
-- | 
pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO = VkStructureType 10
-- | 
pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = VkStructureType 11
-- | 
pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = VkStructureType 12
-- | 
pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = VkStructureType 13
-- | 
pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = VkStructureType 14
-- | 
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = VkStructureType 15
-- | 
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = VkStructureType 16
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = VkStructureType 17
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = VkStructureType 18
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = VkStructureType 19
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = VkStructureType 20
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = VkStructureType 21
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = VkStructureType 22
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = VkStructureType 23
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = VkStructureType 24
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = VkStructureType 25
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = VkStructureType 26
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = VkStructureType 27
-- | 
pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = VkStructureType 28
-- | 
pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = VkStructureType 29
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = VkStructureType 30
-- | 
pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = VkStructureType 31
-- | 
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = VkStructureType 32
-- | 
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = VkStructureType 33
-- | 
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = VkStructureType 34
-- | 
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = VkStructureType 35
-- | 
pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = VkStructureType 36
-- | 
pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = VkStructureType 37
-- | 
pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = VkStructureType 38
-- | 
pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = VkStructureType 39
-- | 
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = VkStructureType 40
-- | 
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = VkStructureType 41
-- | 
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = VkStructureType 42
-- | 
pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = VkStructureType 43
-- | 
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = VkStructureType 44
-- | 
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = VkStructureType 45
-- | 
pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER = VkStructureType 46
-- | 
pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = VkStructureType 47
-- | 
pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = VkStructureType 48


-- | 
newtype VkSubpassContents = VkSubpassContents Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_SUBPASS_CONTENTS_INLINE = VkSubpassContents 0
-- | 
pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = VkSubpassContents 1


-- | Error and return codes
newtype VkResult = VkResult Int deriving (Eq, Ord, Show, Storable)

-- | Command completed successfully
pattern VK_SUCCESS = VkResult 0
-- | A fence or query has not yet completed
pattern VK_NOT_READY = VkResult 1
-- | A wait operation has not completed in the specified time
pattern VK_TIMEOUT = VkResult 2
-- | An event is signaled
pattern VK_EVENT_SET = VkResult 3
-- | An event is unsignalled
pattern VK_EVENT_RESET = VkResult 4
-- | A return array was too small for the result
pattern VK_INCOMPLETE = VkResult 5
-- | A host memory allocation has failed
pattern VK_ERROR_OUT_OF_HOST_MEMORY = VkResult (-1)
-- | A device memory allocation has failed
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY = VkResult (-2)
-- | Initialization of a object has failed
pattern VK_ERROR_INITIALIZATION_FAILED = VkResult (-3)
-- | The logical device has been lost. See <<devsandqueues-lost-device>>
pattern VK_ERROR_DEVICE_LOST = VkResult (-4)
-- | Mapping of a memory object has failed
pattern VK_ERROR_MEMORY_MAP_FAILED = VkResult (-5)
-- | Layer specified does not exist
pattern VK_ERROR_LAYER_NOT_PRESENT = VkResult (-6)
-- | Extension specified does not exist
pattern VK_ERROR_EXTENSION_NOT_PRESENT = VkResult (-7)
-- | Requested feature is not available on this device
pattern VK_ERROR_FEATURE_NOT_PRESENT = VkResult (-8)
-- | Unable to find a Vulkan driver
pattern VK_ERROR_INCOMPATIBLE_DRIVER = VkResult (-9)
-- | Too many objects of the type have already been created
pattern VK_ERROR_TOO_MANY_OBJECTS = VkResult (-10)
-- | Requested format is not supported on this device
pattern VK_ERROR_FORMAT_NOT_SUPPORTED = VkResult (-11)


-- | 
newtype VkDynamicState = VkDynamicState Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_DYNAMIC_STATE_VIEWPORT = VkDynamicState 0
-- | 
pattern VK_DYNAMIC_STATE_SCISSOR = VkDynamicState 1
-- | 
pattern VK_DYNAMIC_STATE_LINE_WIDTH = VkDynamicState 2
-- | 
pattern VK_DYNAMIC_STATE_DEPTH_BIAS = VkDynamicState 3
-- | 
pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS = VkDynamicState 4
-- | 
pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS = VkDynamicState 5
-- | 
pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = VkDynamicState 6
-- | 
pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = VkDynamicState 7
-- | 
pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE = VkDynamicState 8


-- | 
newtype VkQueueFlagBits = VkQueueFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Queue supports graphics operations
pattern VK_QUEUE_GRAPHICS_BIT = VkQueueFlagBits 1
-- | Queue supports compute operations
pattern VK_QUEUE_COMPUTE_BIT = VkQueueFlagBits 2
-- | Queue supports transfer operations
pattern VK_QUEUE_TRANSFER_BIT = VkQueueFlagBits 4
-- | Queue supports sparse resource memory management operations
pattern VK_QUEUE_SPARSE_BINDING_BIT = VkQueueFlagBits 8


-- | 
newtype VkMemoryPropertyFlagBits = VkMemoryPropertyFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | If otherwise stated, then allocate memory on device
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = VkMemoryPropertyFlagBits 1
-- | Memory is mappable by host
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = VkMemoryPropertyFlagBits 2
-- | Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = VkMemoryPropertyFlagBits 4
-- | Memory will be cached by the host
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT = VkMemoryPropertyFlagBits 8
-- | Memory may be allocated by the driver when it is required
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = VkMemoryPropertyFlagBits 16


-- | 
newtype VkMemoryHeapFlagBits = VkMemoryHeapFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | If set, heap represents device memory
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapFlagBits 1


-- | 
newtype VkAccessFlagBits = VkAccessFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Controls coherency of indirect command reads
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessFlagBits 1
-- | Controls coherency of index reads
pattern VK_ACCESS_INDEX_READ_BIT = VkAccessFlagBits 2
-- | Controls coherency of vertex attribute reads
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessFlagBits 4
-- | Controls coherency of uniform buffer reads
pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessFlagBits 8
-- | Controls coherency of input attachment reads
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessFlagBits 16
-- | Controls coherency of shader reads
pattern VK_ACCESS_SHADER_READ_BIT = VkAccessFlagBits 32
-- | Controls coherency of shader writes
pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessFlagBits 64
-- | Controls coherency of color attachment reads
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessFlagBits 128
-- | Controls coherency of color attachment writes
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 256
-- | Controls coherency of depth/stencil attachment reads
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = VkAccessFlagBits 512
-- | Controls coherency of depth/stencil attachment writes
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 1024
-- | Controls coherency of transfer reads
pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessFlagBits 2048
-- | Controls coherency of transfer writes
pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessFlagBits 4096
-- | Controls coherency of host reads
pattern VK_ACCESS_HOST_READ_BIT = VkAccessFlagBits 8192
-- | Controls coherency of host writes
pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessFlagBits 16384
-- | Controls coherency of memory reads
pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessFlagBits 32768
-- | Controls coherency of memory writes
pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessFlagBits 65536


-- | 
newtype VkBufferUsageFlagBits = VkBufferUsageFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Can be used as a source of transfer operations
pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT = VkBufferUsageFlagBits 1
-- | Can be used as a destination of transfer operations
pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT = VkBufferUsageFlagBits 2
-- | Can be used as TBO
pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 4
-- | Can be used as IBO
pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = VkBufferUsageFlagBits 8
-- | Can be used as UBO
pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT = VkBufferUsageFlagBits 16
-- | Can be used as SSBO
pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT = VkBufferUsageFlagBits 32
-- | Can be used as source of fixed-function index fetch (index buffer)
pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT = VkBufferUsageFlagBits 64
-- | Can be used as source of fixed-function vertex fetch (VBO)
pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT = VkBufferUsageFlagBits 128
-- | Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)
pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT = VkBufferUsageFlagBits 256


-- | 
newtype VkBufferCreateFlagBits = VkBufferCreateFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Buffer should support sparse backing
pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT = VkBufferCreateFlagBits 1
-- | Buffer should support sparse backing with partial residency
pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT = VkBufferCreateFlagBits 2
-- | Buffer should support constent data access to physical memory ranges mapped into multiple locations of sparse buffers
pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT = VkBufferCreateFlagBits 4


-- | 
newtype VkShaderStageFlagBits = VkShaderStageFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_SHADER_STAGE_VERTEX_BIT = VkShaderStageFlagBits 1
-- | 
pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = VkShaderStageFlagBits 2
-- | 
pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = VkShaderStageFlagBits 4
-- | 
pattern VK_SHADER_STAGE_GEOMETRY_BIT = VkShaderStageFlagBits 8
-- | 
pattern VK_SHADER_STAGE_FRAGMENT_BIT = VkShaderStageFlagBits 16
-- | 
pattern VK_SHADER_STAGE_COMPUTE_BIT = VkShaderStageFlagBits 32
-- | 
pattern VK_SHADER_STAGE_ALL_GRAPHICS = VkShaderStageFlagBits 0x0000001F
-- | 
pattern VK_SHADER_STAGE_ALL = VkShaderStageFlagBits 0x7FFFFFFF


-- | 
newtype VkImageUsageFlagBits = VkImageUsageFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Can be used as a source of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlagBits 1
-- | Can be used as a destination of transfer operations
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlagBits 2
-- | Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlagBits 4
-- | Can be used as storage image (STORAGE_IMAGE descriptor type)
pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlagBits 8
-- | Can be used as framebuffer color attachment
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VkImageUsageFlagBits 16
-- | Can be used as framebuffer depth/stencil attachment
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VkImageUsageFlagBits 32
-- | Image data not needed outside of rendering
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VkImageUsageFlagBits 64
-- | Can be used as framebuffer input attachment
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VkImageUsageFlagBits 128


-- | 
newtype VkImageCreateFlagBits = VkImageCreateFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Image should support sparse backing
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT = VkImageCreateFlagBits 1
-- | Image should support sparse backing with partial residency
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = VkImageCreateFlagBits 2
-- | Image should support constent data access to physical memory ranges mapped into multiple locations of sparse images
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = VkImageCreateFlagBits 4
-- | Allows image views to have different format than the base image
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = VkImageCreateFlagBits 8
-- | Allows creating image views with cube type from the created image
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = VkImageCreateFlagBits 16


-- | 
newtype VkPipelineCreateFlagBits = VkPipelineCreateFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = VkPipelineCreateFlagBits 1
-- | 
pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = VkPipelineCreateFlagBits 2
-- | 
pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT = VkPipelineCreateFlagBits 4


-- | 
newtype VkColorComponentFlagBits = VkColorComponentFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_COLOR_COMPONENT_R_BIT = VkColorComponentFlagBits 1
-- | 
pattern VK_COLOR_COMPONENT_G_BIT = VkColorComponentFlagBits 2
-- | 
pattern VK_COLOR_COMPONENT_B_BIT = VkColorComponentFlagBits 4
-- | 
pattern VK_COLOR_COMPONENT_A_BIT = VkColorComponentFlagBits 8


-- | 
newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 1


-- | 
newtype VkFormatFeatureFlagBits = VkFormatFeatureFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = VkFormatFeatureFlagBits 1
-- | Format can be used for storage images (STORAGE_IMAGE descriptor type)
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = VkFormatFeatureFlagBits 2
-- | Format supports atomic operations in case it's used for storage images
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = VkFormatFeatureFlagBits 4
-- | Format can be used for uniform texel buffers (TBOs)
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 8
-- | Format can be used for storage texel buffers (IBOs)
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 16
-- | Format supports atomic operations in case it's used for storage texel buffers
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = VkFormatFeatureFlagBits 32
-- | Format can be used for vertex buffers (VBOs)
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = VkFormatFeatureFlagBits 64
-- | Format can be used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = VkFormatFeatureFlagBits 128
-- | Format supports blending in case it's used for color attachment images
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = VkFormatFeatureFlagBits 256
-- | Format can be used for depth/stencil attachment images
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = VkFormatFeatureFlagBits 512
-- | Format can be used as the source image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT = VkFormatFeatureFlagBits 1024
-- | Format can be used as the destination image of blits with vkCmdBlitImage
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT = VkFormatFeatureFlagBits 2048
-- | Format can be filtered with VK_FILTER_LINEAR when being sampled
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = VkFormatFeatureFlagBits 4096


-- | 
newtype VkQueryControlFlagBits = VkQueryControlFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Require precise results to be collected by the query
pattern VK_QUERY_CONTROL_PRECISE_BIT = VkQueryControlFlagBits 1


-- | 
newtype VkQueryResultFlagBits = VkQueryResultFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Results of the queries are written to the destination buffer as 64-bit values
pattern VK_QUERY_RESULT_64_BIT = VkQueryResultFlagBits 1
-- | Results of the queries are waited on before proceeding with the result copy
pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultFlagBits 2
-- | Besides the results of the query, the availability of the results is also written
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = VkQueryResultFlagBits 4
-- | Copy the partial results of the query even if the final results aren't available
pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultFlagBits 8


-- | 
newtype VkCommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT = VkCommandBufferUsageFlagBits 1
-- | 
pattern VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT = VkCommandBufferUsageFlagBits 2
-- | Command buffer may be submitted/executed more than once simultaneously
pattern VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT = VkCommandBufferUsageFlagBits 4


-- | 
newtype VkQueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = VkQueryPipelineStatisticFlagBits 1
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 2
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 4
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 8
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 16
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 32
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 64
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 128
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = VkQueryPipelineStatisticFlagBits 256
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 512
-- | Optional
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 1024


-- | 
newtype VkImageAspectFlagBits = VkImageAspectFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_IMAGE_ASPECT_COLOR_BIT = VkImageAspectFlagBits 1
-- | 
pattern VK_IMAGE_ASPECT_DEPTH_BIT = VkImageAspectFlagBits 2
-- | 
pattern VK_IMAGE_ASPECT_STENCIL_BIT = VkImageAspectFlagBits 4
-- | 
pattern VK_IMAGE_ASPECT_METADATA_BIT = VkImageAspectFlagBits 8


-- | 
newtype VkSparseImageFormatFlagBits = VkSparseImageFormatFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Image uses a single miptail region for all array layers
pattern VK_SPARSE_IMAGE_FORMAT_SINGLE_MIPTAIL_BIT = VkSparseImageFormatFlagBits 1
-- | Image requires mip level dimensions to be an integer multiple of the sparse image block dimensions for non-miptail levels.
pattern VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT = VkSparseImageFormatFlagBits 2
-- | Image uses a non-standard sparse image block dimensions
pattern VK_SPARSE_IMAGE_FORMAT_NONSTANDARD_BLOCK_SIZE_BIT = VkSparseImageFormatFlagBits 4


-- | 
newtype VkSparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Operation binds resource metadata to memory
pattern VK_SPARSE_MEMORY_BIND_METADATA_BIT = VkSparseMemoryBindFlagBits 1


-- | 
newtype VkPipelineStageFlagBits = VkPipelineStageFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Before subsequent commands are processed
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = VkPipelineStageFlagBits 1
-- | Draw/DispatchIndirect command fetch
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT = VkPipelineStageFlagBits 2
-- | Vertex/index fetch
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = VkPipelineStageFlagBits 4
-- | Vertex shading
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = VkPipelineStageFlagBits 8
-- | Tessellation control shading
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = VkPipelineStageFlagBits 16
-- | Tessellation evaluation shading
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = VkPipelineStageFlagBits 32
-- | Geometry shading
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT = VkPipelineStageFlagBits 64
-- | Fragment shading
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = VkPipelineStageFlagBits 128
-- | Early fragment (depth and stencil) tests
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 256
-- | Late fragment (depth and stencil) tests
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 512
-- | Color attachment writes
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = VkPipelineStageFlagBits 1024
-- | Compute shading
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT = VkPipelineStageFlagBits 2048
-- | Transfer/copy operations
pattern VK_PIPELINE_STAGE_TRANSFER_BIT = VkPipelineStageFlagBits 4096
-- | After previous commands have completed
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = VkPipelineStageFlagBits 8192
-- | Indicates host (CPU) is a source/sink of the dependency
pattern VK_PIPELINE_STAGE_HOST_BIT = VkPipelineStageFlagBits 16384
-- | All stages of the graphics pipeline
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT = VkPipelineStageFlagBits 32768
-- | All stages supported on the queue
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT = VkPipelineStageFlagBits 65536


-- | 
newtype VkCommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Command buffers have a short lifetime
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = VkCommandPoolCreateFlagBits 1
-- | Command buffers may release their memory individually
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = VkCommandPoolCreateFlagBits 2


-- | 
newtype VkCommandPoolResetFlagBits = VkCommandPoolResetFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Release resources owned by the pool
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = VkCommandPoolResetFlagBits 1


-- | 
newtype VkCommandBufferResetFlagBits = VkCommandBufferResetFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Release resources owned by the buffer
pattern VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT = VkCommandBufferResetFlagBits 1


-- | 
newtype VkSampleCountFlagBits = VkSampleCountFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Sample count 1 supported
pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 1
-- | Sample count 2 supported
pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 2
-- | Sample count 4 supported
pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 4
-- | Sample count 8 supported
pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 8
-- | Sample count 16 supported
pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 16
-- | Sample count 32 supported
pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 32
-- | Sample count 64 supported
pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 64


-- | 
newtype VkAttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | The attachment may alias physical memory of another attachment in the same render pass
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VkAttachmentDescriptionFlagBits 1


-- | 
newtype VkStencilFaceFlagBits = VkStencilFaceFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Front face
pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlagBits 1
-- | Back face
pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlagBits 2
-- | Front and back faces
pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlagBits 0x00000003


-- | 
newtype VkDescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Descriptor sets may be freed individually
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = VkDescriptorPoolCreateFlagBits 1


-- | 
newtype VkDependencyFlagBits = VkDependencyFlagBits Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | Dependency is per pixel region 
pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlagBits 1


-- | 
newtype VkPresentModeKHR = VkPresentModeKHR Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0
-- | 
pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1
-- | 
pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2
-- | 
pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3


-- | 
newtype VkColorSpaceKHR = VkColorSpaceKHR Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0


-- | 
newtype VkDisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 1
-- | 
pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 2
-- | 
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 4
-- | 
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 8


-- | 
newtype VkCompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = VkCompositeAlphaFlagBitsKHR 1
-- | 
pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 2
-- | 
pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 4
-- | 
pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = VkCompositeAlphaFlagBitsKHR 8


-- | 
newtype VkSurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = VkSurfaceTransformFlagBitsKHR 1
-- | 
pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 2
-- | 
pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 4
-- | 
pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 8
-- | 
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = VkSurfaceTransformFlagBitsKHR 16
-- | 
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 32
-- | 
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 64
-- | 
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 128
-- | 
pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = VkSurfaceTransformFlagBitsKHR 256


-- | 
newtype VkDebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT Int deriving (Eq, Ord, Bits, FiniteBits, Show, Storable)

-- | 
pattern VK_DEBUG_REPORT_INFORMATION_BIT_EXT = VkDebugReportFlagBitsEXT 1
-- | 
pattern VK_DEBUG_REPORT_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 2
-- | 
pattern VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = VkDebugReportFlagBitsEXT 4
-- | 
pattern VK_DEBUG_REPORT_ERROR_BIT_EXT = VkDebugReportFlagBitsEXT 8
-- | 
pattern VK_DEBUG_REPORT_DEBUG_BIT_EXT = VkDebugReportFlagBitsEXT 16


-- | 
newtype VkDebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT = VkDebugReportObjectTypeEXT 0
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT = VkDebugReportObjectTypeEXT 1
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT = VkDebugReportObjectTypeEXT 2
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT = VkDebugReportObjectTypeEXT 3
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT = VkDebugReportObjectTypeEXT 4
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT = VkDebugReportObjectTypeEXT 5
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT = VkDebugReportObjectTypeEXT 6
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT = VkDebugReportObjectTypeEXT 7
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT = VkDebugReportObjectTypeEXT 8
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT = VkDebugReportObjectTypeEXT 9
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT = VkDebugReportObjectTypeEXT 10
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT = VkDebugReportObjectTypeEXT 11
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT = VkDebugReportObjectTypeEXT 12
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT = VkDebugReportObjectTypeEXT 13
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT = VkDebugReportObjectTypeEXT 14
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT = VkDebugReportObjectTypeEXT 15
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT = VkDebugReportObjectTypeEXT 16
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT = VkDebugReportObjectTypeEXT 17
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT = VkDebugReportObjectTypeEXT 18
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT = VkDebugReportObjectTypeEXT 19
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT = VkDebugReportObjectTypeEXT 20
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT = VkDebugReportObjectTypeEXT 21
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT = VkDebugReportObjectTypeEXT 22
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT = VkDebugReportObjectTypeEXT 23
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT = VkDebugReportObjectTypeEXT 24
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT = VkDebugReportObjectTypeEXT 25
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT = VkDebugReportObjectTypeEXT 26
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT = VkDebugReportObjectTypeEXT 27
-- | 
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT = VkDebugReportObjectTypeEXT 28


-- | 
newtype VkDebugReportErrorEXT = VkDebugReportErrorEXT Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_DEBUG_REPORT_ERROR_NONE_EXT = VkDebugReportErrorEXT 0
-- | 
pattern VK_DEBUG_REPORT_ERROR_CALLBACK_REF_EXT = VkDebugReportErrorEXT 1


-- | 
newtype VkRasterizationOrderAMD = VkRasterizationOrderAMD Int deriving (Eq, Ord, Show, Storable)

-- | 
pattern VK_RASTERIZATION_ORDER_STRICT_AMD = VkRasterizationOrderAMD 0
-- | 
pattern VK_RASTERIZATION_ORDER_RELAXED_AMD = VkRasterizationOrderAMD 1



foreign import ccall unsafe "dynamic" ffi_vkCreateInstance :: FunPtr (Ptr VkInstanceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkInstance -> IO VkResult) -> (Ptr VkInstanceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkInstance -> IO VkResult)

-- | @vkCreateInstance pCreateInfo pAllocator pInstance@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_INITIALIZATION_FAILED,VK_ERROR_LAYER_NOT_PRESENT,VK_ERROR_EXTENSION_NOT_PRESENT,VK_ERROR_INCOMPATIBLE_DRIVER q: rp: cbl: iesp:[]
vkCreateInstance
  :: MonadIO m
  => Ptr VkInstanceCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkInstance
  -- ^ `pInstance` 
  -> m VkResult
vkCreateInstance pCreateInfo pAllocator pInstance =
  liftIO (ffi_vkCreateInstance fp_vkCreateInstance pCreateInfo pAllocator pInstance)


foreign import ccall unsafe "dynamic" ffi_vkDestroyInstance :: FunPtr (VkInstance -> Ptr VkAllocationCallbacks -> IO ()) -> (VkInstance -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyInstance vulkan pAllocator@
-- 
-- == Validaty
-- * All child objects created using pname:instance must: have been destroyed prior to destroying pname:instance
-- * If sname:VkAllocationCallbacks were provided when pname:instance was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:instance was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyInstance
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyInstance vk vulkan pAllocator =
  liftIO (ffi_vkDestroyInstance (fp_vkDestroyInstance vk) vulkan pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkEnumeratePhysicalDevices :: FunPtr (VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO VkResult) -> (VkInstance -> Ptr Word32 -> Ptr VkPhysicalDevice -> IO VkResult)

-- | @vkEnumeratePhysicalDevices vulkan pPhysicalDeviceCount pPhysicalDevices@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_INITIALIZATION_FAILED q: rp: cbl: iesp:[]
vkEnumeratePhysicalDevices
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> Ptr Word32
  -- ^ `pPhysicalDeviceCount`  Optional=false,true.
  -> Ptr VkPhysicalDevice
  -- ^ `pPhysicalDevices`  Can be `nullPtr`. Length: pPhysicalDeviceCount.
  -> m VkResult
vkEnumeratePhysicalDevices vk vulkan pPhysicalDeviceCount pPhysicalDevices =
  liftIO (ffi_vkEnumeratePhysicalDevices (fp_vkEnumeratePhysicalDevices vk) vulkan pPhysicalDeviceCount pPhysicalDevices)


foreign import ccall unsafe "dynamic" ffi_vkGetDeviceProcAddr :: FunPtr (VkDevice -> Ptr CChar -> IO PFN_vkVoidFunction) -> (VkDevice -> Ptr CChar -> IO PFN_vkVoidFunction)

-- | @vkGetDeviceProcAddr device pName@
-- 
-- == Validaty
-- * pname:pName must: be the name of a supported command that has a first parameter of type sname:VkDevice, sname:VkQueue or sname:VkCommandBuffer, either in the core API or an enabled extension
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetDeviceProcAddr
  :: MonadIO m => VulkanSetup
  -> VkDevice
  -- ^ `device` 
  -> Ptr CChar
  -- ^ `pName`  Const. Length: null-terminated.
  -> m PFN_vkVoidFunction
vkGetDeviceProcAddr vk device pName =
  liftIO (ffi_vkGetDeviceProcAddr (fp_vkGetDeviceProcAddr vk) device pName)


foreign import ccall unsafe "dynamic" ffi_vkGetInstanceProcAddr :: FunPtr (VkInstance -> Ptr CChar -> IO PFN_vkVoidFunction) -> (VkInstance -> Ptr CChar -> IO PFN_vkVoidFunction)

-- | @vkGetInstanceProcAddr vulkan pName@
-- 
-- == Validaty
-- * If pname:instance is `NULL`, pname:pName must: be one of: fname:vkEnumerateInstanceExtensionProperties, fname:vkEnumerateInstanceLayerProperties or fname:vkCreateInstance
-- * If pname:instance is not `NULL`, pname:pName must: be the name of a core command or a command from an enabled extension, other than: fname:vkEnumerateInstanceExtensionProperties, fname:vkEnumerateInstanceLayerProperties or fname:vkCreateInstance
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetInstanceProcAddr
  :: MonadIO m
  => VkInstance
  -- ^ `vulkan`  Can be `nullPtr`.
  -> Ptr CChar
  -- ^ `pName`  Const. Length: null-terminated.
  -> m PFN_vkVoidFunction
vkGetInstanceProcAddr vulkan pName =
  liftIO (ffi_vkGetInstanceProcAddr fp_vkGetInstanceProcAddr vulkan pName)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceProperties :: FunPtr (VkPhysicalDevice -> Ptr VkPhysicalDeviceProperties -> IO ()) -> (VkPhysicalDevice -> Ptr VkPhysicalDeviceProperties -> IO ())

-- | @vkGetPhysicalDeviceProperties physicalDevice pProperties@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceProperties
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Ptr VkPhysicalDeviceProperties
  -- ^ `pProperties` 
  -> m ()
vkGetPhysicalDeviceProperties vk physicalDevice pProperties =
  liftIO (ffi_vkGetPhysicalDeviceProperties (fp_vkGetPhysicalDeviceProperties vk) physicalDevice pProperties)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceQueueFamilyProperties :: FunPtr (VkPhysicalDevice -> Ptr Word32 -> Ptr VkQueueFamilyProperties -> IO ()) -> (VkPhysicalDevice -> Ptr Word32 -> Ptr VkQueueFamilyProperties -> IO ())

-- | @vkGetPhysicalDeviceQueueFamilyProperties physicalDevice pQueueFamilyPropertyCount pQueueFamilyProperties@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceQueueFamilyProperties
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Ptr Word32
  -- ^ `pQueueFamilyPropertyCount`  Optional=false,true.
  -> Ptr VkQueueFamilyProperties
  -- ^ `pQueueFamilyProperties`  Can be `nullPtr`. Length: pQueueFamilyPropertyCount.
  -> m ()
vkGetPhysicalDeviceQueueFamilyProperties vk physicalDevice pQueueFamilyPropertyCount pQueueFamilyProperties =
  liftIO (ffi_vkGetPhysicalDeviceQueueFamilyProperties (fp_vkGetPhysicalDeviceQueueFamilyProperties vk) physicalDevice pQueueFamilyPropertyCount pQueueFamilyProperties)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceMemoryProperties :: FunPtr (VkPhysicalDevice -> Ptr VkPhysicalDeviceMemoryProperties -> IO ()) -> (VkPhysicalDevice -> Ptr VkPhysicalDeviceMemoryProperties -> IO ())

-- | @vkGetPhysicalDeviceMemoryProperties physicalDevice pMemoryProperties@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceMemoryProperties
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Ptr VkPhysicalDeviceMemoryProperties
  -- ^ `pMemoryProperties` 
  -> m ()
vkGetPhysicalDeviceMemoryProperties vk physicalDevice pMemoryProperties =
  liftIO (ffi_vkGetPhysicalDeviceMemoryProperties (fp_vkGetPhysicalDeviceMemoryProperties vk) physicalDevice pMemoryProperties)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceFeatures :: FunPtr (VkPhysicalDevice -> Ptr VkPhysicalDeviceFeatures -> IO ()) -> (VkPhysicalDevice -> Ptr VkPhysicalDeviceFeatures -> IO ())

-- | @vkGetPhysicalDeviceFeatures physicalDevice pFeatures@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceFeatures
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Ptr VkPhysicalDeviceFeatures
  -- ^ `pFeatures` 
  -> m ()
vkGetPhysicalDeviceFeatures vk physicalDevice pFeatures =
  liftIO (ffi_vkGetPhysicalDeviceFeatures (fp_vkGetPhysicalDeviceFeatures vk) physicalDevice pFeatures)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceFormatProperties :: FunPtr (VkPhysicalDevice -> VkFormat -> Ptr VkFormatProperties -> IO ()) -> (VkPhysicalDevice -> VkFormat -> Ptr VkFormatProperties -> IO ())

-- | @vkGetPhysicalDeviceFormatProperties physicalDevice format pFormatProperties@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceFormatProperties
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> VkFormat
  -- ^ `format` 
  -> Ptr VkFormatProperties
  -- ^ `pFormatProperties` 
  -> m ()
vkGetPhysicalDeviceFormatProperties vk physicalDevice format pFormatProperties =
  liftIO (ffi_vkGetPhysicalDeviceFormatProperties (fp_vkGetPhysicalDeviceFormatProperties vk) physicalDevice format pFormatProperties)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceImageFormatProperties :: FunPtr (VkPhysicalDevice -> VkFormat -> VkImageType -> VkImageTiling -> VkImageUsageFlags -> VkImageCreateFlags -> Ptr VkImageFormatProperties -> IO VkResult) -> (VkPhysicalDevice -> VkFormat -> VkImageType -> VkImageTiling -> VkImageUsageFlags -> VkImageCreateFlags -> Ptr VkImageFormatProperties -> IO VkResult)

-- | @vkGetPhysicalDeviceImageFormatProperties physicalDevice format imageType tiling usage flags pImageFormatProperties@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_FORMAT_NOT_SUPPORTED q: rp: cbl: iesp:[]
vkGetPhysicalDeviceImageFormatProperties
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> VkFormat
  -- ^ `format` 
  -> VkImageType
  -- ^ `imageType` 
  -> VkImageTiling
  -- ^ `tiling` 
  -> VkImageUsageFlags
  -- ^ `usage` 
  -> VkImageCreateFlags
  -- ^ `flags`  Can be `nullPtr`.
  -> Ptr VkImageFormatProperties
  -- ^ `pImageFormatProperties` 
  -> m VkResult
vkGetPhysicalDeviceImageFormatProperties vk physicalDevice format imageType tiling usage flags pImageFormatProperties =
  liftIO (ffi_vkGetPhysicalDeviceImageFormatProperties (fp_vkGetPhysicalDeviceImageFormatProperties vk) physicalDevice format imageType tiling usage flags pImageFormatProperties)


foreign import ccall unsafe "dynamic" ffi_vkCreateDevice :: FunPtr (VkPhysicalDevice -> Ptr VkDeviceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDevice -> IO VkResult) -> (VkPhysicalDevice -> Ptr VkDeviceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDevice -> IO VkResult)

-- | @vkCreateDevice physicalDevice pCreateInfo pAllocator pDevice@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_INITIALIZATION_FAILED,VK_ERROR_LAYER_NOT_PRESENT,VK_ERROR_EXTENSION_NOT_PRESENT,VK_ERROR_FEATURE_NOT_PRESENT,VK_ERROR_TOO_MANY_OBJECTS,VK_ERROR_DEVICE_LOST q: rp: cbl: iesp:[]
vkCreateDevice
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Ptr VkDeviceCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkDevice
  -- ^ `pDevice` 
  -> m VkResult
vkCreateDevice vk physicalDevice pCreateInfo pAllocator pDevice =
  liftIO (ffi_vkCreateDevice (fp_vkCreateDevice vk) physicalDevice pCreateInfo pAllocator pDevice)


foreign import ccall unsafe "dynamic" ffi_vkDestroyDevice :: FunPtr (VkDevice -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyDevice device pAllocator@
-- 
-- == Validaty
-- * All child objects created on pname:device must: have been destroyed prior to destroying pname:device
-- * If sname:VkAllocationCallbacks were provided when pname:device was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:device was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyDevice
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyDevice vk device pAllocator =
  liftIO (ffi_vkDestroyDevice (fp_vkDestroyDevice vk) device pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkEnumerateInstanceLayerProperties :: FunPtr (Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult) -> (Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult)

-- | @vkEnumerateInstanceLayerProperties pPropertyCount pProperties@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkEnumerateInstanceLayerProperties
  :: MonadIO m
  => Ptr Word32
  -- ^ `pPropertyCount`  Optional=false,true.
  -> Ptr VkLayerProperties
  -- ^ `pProperties`  Can be `nullPtr`. Length: pPropertyCount.
  -> m VkResult
vkEnumerateInstanceLayerProperties pPropertyCount pProperties =
  liftIO (ffi_vkEnumerateInstanceLayerProperties fp_vkEnumerateInstanceLayerProperties pPropertyCount pProperties)


foreign import ccall unsafe "dynamic" ffi_vkEnumerateInstanceExtensionProperties :: FunPtr (Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult) -> (Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult)

-- | @vkEnumerateInstanceExtensionProperties pLayerName pPropertyCount pProperties@
-- 
-- == Validaty
-- * If pname:pLayerName is not `NULL`, it must: be the name of an instance layer returned by flink:vkEnumerateInstanceLayerProperties
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkEnumerateInstanceExtensionProperties
  :: MonadIO m
  => Ptr CChar
  -- ^ `pLayerName`  Can be `nullPtr`. Const. Length: null-terminated.
  -> Ptr Word32
  -- ^ `pPropertyCount`  Optional=false,true.
  -> Ptr VkExtensionProperties
  -- ^ `pProperties`  Can be `nullPtr`. Length: pPropertyCount.
  -> m VkResult
vkEnumerateInstanceExtensionProperties pLayerName pPropertyCount pProperties =
  liftIO (ffi_vkEnumerateInstanceExtensionProperties fp_vkEnumerateInstanceExtensionProperties pLayerName pPropertyCount pProperties)


foreign import ccall unsafe "dynamic" ffi_vkEnumerateDeviceLayerProperties :: FunPtr (VkPhysicalDevice -> Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult) -> (VkPhysicalDevice -> Ptr Word32 -> Ptr VkLayerProperties -> IO VkResult)

-- | @vkEnumerateDeviceLayerProperties physicalDevice pPropertyCount pProperties@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkEnumerateDeviceLayerProperties
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice`  Optional=false,true.
  -> Ptr Word32
  -- ^ `pPropertyCount`  Optional=false,true.
  -> Ptr VkLayerProperties
  -- ^ `pProperties`  Can be `nullPtr`. Length: pPropertyCount.
  -> m VkResult
vkEnumerateDeviceLayerProperties vk physicalDevice pPropertyCount pProperties =
  liftIO (ffi_vkEnumerateDeviceLayerProperties (fp_vkEnumerateDeviceLayerProperties vk) physicalDevice pPropertyCount pProperties)


foreign import ccall unsafe "dynamic" ffi_vkEnumerateDeviceExtensionProperties :: FunPtr (VkPhysicalDevice -> Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult) -> (VkPhysicalDevice -> Ptr CChar -> Ptr Word32 -> Ptr VkExtensionProperties -> IO VkResult)

-- | @vkEnumerateDeviceExtensionProperties physicalDevice pLayerName pPropertyCount pProperties@
-- 
-- == Validaty
-- * If pname:pLayerName is not `NULL`, it must: be the name of a device layer returned by flink:vkEnumerateDeviceLayerProperties
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkEnumerateDeviceExtensionProperties
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Ptr CChar
  -- ^ `pLayerName`  Can be `nullPtr`. Const. Length: null-terminated.
  -> Ptr Word32
  -- ^ `pPropertyCount`  Optional=false,true.
  -> Ptr VkExtensionProperties
  -- ^ `pProperties`  Can be `nullPtr`. Length: pPropertyCount.
  -> m VkResult
vkEnumerateDeviceExtensionProperties vk physicalDevice pLayerName pPropertyCount pProperties =
  liftIO (ffi_vkEnumerateDeviceExtensionProperties (fp_vkEnumerateDeviceExtensionProperties vk) physicalDevice pLayerName pPropertyCount pProperties)


foreign import ccall unsafe "dynamic" ffi_vkGetDeviceQueue :: FunPtr (VkDevice -> Word32 -> Word32 -> Ptr VkQueue -> IO ()) -> (VkDevice -> Word32 -> Word32 -> Ptr VkQueue -> IO ())

-- | @vkGetDeviceQueue device queueFamilyIndex queueIndex pQueue@
-- 
-- == Validaty
-- * pname:queueFamilyIndex must: be one of the queue family indices specified when pname:device was created, via the sname:VkDeviceQueueCreateInfo structure
-- * pname:queueIndex must: be less than the number of queues created for the specified queue family index when pname:device was created, via the pname:queueCount member of the sname:VkDeviceQueueCreateInfo structure
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetDeviceQueue
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Word32
  -- ^ `queueFamilyIndex` 
  -> Word32
  -- ^ `queueIndex` 
  -> Ptr VkQueue
  -- ^ `pQueue` 
  -> m ()
vkGetDeviceQueue vk device queueFamilyIndex queueIndex pQueue =
  liftIO (ffi_vkGetDeviceQueue (fp_vkGetDeviceQueue vk) device queueFamilyIndex queueIndex pQueue)


foreign import ccall unsafe "dynamic" ffi_vkQueueSubmit :: FunPtr (VkQueue -> Word32 -> Ptr VkSubmitInfo -> VkFence -> IO VkResult) -> (VkQueue -> Word32 -> Ptr VkSubmitInfo -> VkFence -> IO VkResult)

-- | @vkQueueSubmit queue submitCount pSubmits fence@
-- 
-- == Validaty
-- * If pname:fence is not sname:VK_NULL_HANDLE, pname:fence must: be unsignalled
-- * If pname:fence is not sname:VK_NULL_HANDLE, pname:fence mustnot: be associated with any other queue command that has not yet completed execution on that queue
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST q: rp: cbl: iesp:[]
vkQueueSubmit
  :: MonadIO m => Vulkan
  -> VkQueue
  -- ^ `queue`  ExternSync=true.
  -> Word32
  -- ^ `submitCount`  Can be `nullPtr`.
  -> Ptr VkSubmitInfo
  -- ^ `pSubmits`  ExternSync=pSubmits[].pWaitSemaphores[],pSubmits[].pSignalSemaphores[]. Const. Length: submitCount.
  -> VkFence
  -- ^ `fence`  Can be `nullPtr`. ExternSync=true.
  -> m VkResult
vkQueueSubmit vk queue submitCount pSubmits fence =
  liftIO (ffi_vkQueueSubmit (fp_vkQueueSubmit vk) queue submitCount pSubmits fence)


foreign import ccall unsafe "dynamic" ffi_vkQueueWaitIdle :: FunPtr (VkQueue -> IO VkResult) -> (VkQueue -> IO VkResult)

-- | @vkQueueWaitIdle queue@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST q: rp: cbl: iesp:[]
vkQueueWaitIdle
  :: MonadIO m => Vulkan
  -> VkQueue
  -- ^ `queue` 
  -> m VkResult
vkQueueWaitIdle vk queue =
  liftIO (ffi_vkQueueWaitIdle (fp_vkQueueWaitIdle vk) queue)


foreign import ccall unsafe "dynamic" ffi_vkDeviceWaitIdle :: FunPtr (VkDevice -> IO VkResult) -> (VkDevice -> IO VkResult)

-- | @vkDeviceWaitIdle device@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST q: rp: cbl: iesp:[]
vkDeviceWaitIdle
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> m VkResult
vkDeviceWaitIdle vk device =
  liftIO (ffi_vkDeviceWaitIdle (fp_vkDeviceWaitIdle vk) device)


foreign import ccall unsafe "dynamic" ffi_vkAllocateMemory :: FunPtr (VkDevice -> Ptr VkMemoryAllocateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDeviceMemory -> IO VkResult) -> (VkDevice -> Ptr VkMemoryAllocateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDeviceMemory -> IO VkResult)

-- | @vkAllocateMemory device pAllocateInfo pAllocator pMemory@
-- 
-- == Validaty
-- * The number of currently valid memory objects, allocated from pname:device, must: be less than sname:VkPhysicalDeviceLimits::pname:maxMemoryAllocationCount
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_TOO_MANY_OBJECTS q: rp: cbl: iesp:[]
vkAllocateMemory
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkMemoryAllocateInfo
  -- ^ `pAllocateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkDeviceMemory
  -- ^ `pMemory` 
  -> m VkResult
vkAllocateMemory vk device pAllocateInfo pAllocator pMemory =
  liftIO (ffi_vkAllocateMemory (fp_vkAllocateMemory vk) device pAllocateInfo pAllocator pMemory)


foreign import ccall unsafe "dynamic" ffi_vkFreeMemory :: FunPtr (VkDevice -> VkDeviceMemory -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkDeviceMemory -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkFreeMemory device memory pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:memory (via images or buffers) must: have completed execution
-- 
-- s: e: q: rp: cbl: iesp:[]
vkFreeMemory
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkDeviceMemory
  -- ^ `memory`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkFreeMemory vk device memory pAllocator =
  liftIO (ffi_vkFreeMemory (fp_vkFreeMemory vk) device memory pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkMapMemory :: FunPtr (VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags -> Ptr (Ptr ()) -> IO VkResult) -> (VkDevice -> VkDeviceMemory -> VkDeviceSize -> VkDeviceSize -> VkMemoryMapFlags -> Ptr (Ptr ()) -> IO VkResult)

-- | @vkMapMemory device memory offset size flags ppData@
-- 
-- == Validaty
-- * pname:memory mustnot: currently be mapped
-- * pname:offset must: be less than the size of pname:memory
-- * If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be greater than `0`
-- * If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be less than or equal to the size of the pname:memory minus pname:offset
-- * pname:memory must: have been created with a memory type that reports ename:VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_MEMORY_MAP_FAILED q: rp: cbl: iesp:[]
vkMapMemory
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkDeviceMemory
  -- ^ `memory`  ExternSync=true.
  -> VkDeviceSize
  -- ^ `offset` 
  -> VkDeviceSize
  -- ^ `size` 
  -> VkMemoryMapFlags
  -- ^ `flags`  Can be `nullPtr`.
  -> Ptr (Ptr ())
  -- ^ `ppData` 
  -> m VkResult
vkMapMemory vk device memory offset size flags ppData =
  liftIO (ffi_vkMapMemory (fp_vkMapMemory vk) device memory offset size flags ppData)


foreign import ccall unsafe "dynamic" ffi_vkUnmapMemory :: FunPtr (VkDevice -> VkDeviceMemory -> IO ()) -> (VkDevice -> VkDeviceMemory -> IO ())

-- | @vkUnmapMemory device memory@
-- 
-- == Validaty
-- * pname:memory must: currently be mapped
-- 
-- s: e: q: rp: cbl: iesp:[]
vkUnmapMemory
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkDeviceMemory
  -- ^ `memory`  ExternSync=true.
  -> m ()
vkUnmapMemory vk device memory =
  liftIO (ffi_vkUnmapMemory (fp_vkUnmapMemory vk) device memory)


foreign import ccall unsafe "dynamic" ffi_vkFlushMappedMemoryRanges :: FunPtr (VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult) -> (VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult)

-- | @vkFlushMappedMemoryRanges device memoryRangeCount pMemoryRanges@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkFlushMappedMemoryRanges
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Word32
  -- ^ `memoryRangeCount` 
  -> Ptr VkMappedMemoryRange
  -- ^ `pMemoryRanges`  Const. Length: memoryRangeCount.
  -> m VkResult
vkFlushMappedMemoryRanges vk device memoryRangeCount pMemoryRanges =
  liftIO (ffi_vkFlushMappedMemoryRanges (fp_vkFlushMappedMemoryRanges vk) device memoryRangeCount pMemoryRanges)


foreign import ccall unsafe "dynamic" ffi_vkInvalidateMappedMemoryRanges :: FunPtr (VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult) -> (VkDevice -> Word32 -> Ptr VkMappedMemoryRange -> IO VkResult)

-- | @vkInvalidateMappedMemoryRanges device memoryRangeCount pMemoryRanges@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkInvalidateMappedMemoryRanges
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Word32
  -- ^ `memoryRangeCount` 
  -> Ptr VkMappedMemoryRange
  -- ^ `pMemoryRanges`  Const. Length: memoryRangeCount.
  -> m VkResult
vkInvalidateMappedMemoryRanges vk device memoryRangeCount pMemoryRanges =
  liftIO (ffi_vkInvalidateMappedMemoryRanges (fp_vkInvalidateMappedMemoryRanges vk) device memoryRangeCount pMemoryRanges)


foreign import ccall unsafe "dynamic" ffi_vkGetDeviceMemoryCommitment :: FunPtr (VkDevice -> VkDeviceMemory -> Ptr VkDeviceSize -> IO ()) -> (VkDevice -> VkDeviceMemory -> Ptr VkDeviceSize -> IO ())

-- | @vkGetDeviceMemoryCommitment device memory pCommittedMemoryInBytes@
-- 
-- == Validaty
-- * pname:memory must: have been created with a memory type that reports ename:VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetDeviceMemoryCommitment
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkDeviceMemory
  -- ^ `memory` 
  -> Ptr VkDeviceSize
  -- ^ `pCommittedMemoryInBytes` 
  -> m ()
vkGetDeviceMemoryCommitment vk device memory pCommittedMemoryInBytes =
  liftIO (ffi_vkGetDeviceMemoryCommitment (fp_vkGetDeviceMemoryCommitment vk) device memory pCommittedMemoryInBytes)


foreign import ccall unsafe "dynamic" ffi_vkGetBufferMemoryRequirements :: FunPtr (VkDevice -> VkBuffer -> Ptr VkMemoryRequirements -> IO ()) -> (VkDevice -> VkBuffer -> Ptr VkMemoryRequirements -> IO ())

-- | @vkGetBufferMemoryRequirements device buffer pMemoryRequirements@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetBufferMemoryRequirements
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkBuffer
  -- ^ `buffer` 
  -> Ptr VkMemoryRequirements
  -- ^ `pMemoryRequirements` 
  -> m ()
vkGetBufferMemoryRequirements vk device buffer pMemoryRequirements =
  liftIO (ffi_vkGetBufferMemoryRequirements (fp_vkGetBufferMemoryRequirements vk) device buffer pMemoryRequirements)


foreign import ccall unsafe "dynamic" ffi_vkBindBufferMemory :: FunPtr (VkDevice -> VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO VkResult) -> (VkDevice -> VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO VkResult)

-- | @vkBindBufferMemory device buffer memory memoryOffset@
-- 
-- == Validaty
-- * pname:buffer mustnot: already be backed by a memory object
-- * pname:buffer mustnot: have been created with any sparse memory binding flags
-- * pname:memoryOffset must: be less than the size of pname:memory
-- * If pname:buffer was created with the ename:VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT or ename:VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT, pname:memoryOffset must: be a multiple of sname:VkPhysicalDeviceLimits::pname:minTexelBufferOffsetAlignment
-- * If pname:buffer was created with the ename:VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, pname:memoryOffset must: be a multiple of sname:VkPhysicalDeviceLimits::pname:minUniformBufferOffsetAlignment
-- * If pname:buffer was created with the ename:VK_BUFFER_USAGE_STORAGE_BUFFER_BIT, pname:memoryOffset must: be a multiple of sname:VkPhysicalDeviceLimits::pname:minStorageBufferOffsetAlignment
-- * pname:memory must: have been allocated using one of the memory types allowed in the pname:memoryTypeBits member of the sname:VkMemoryRequirements structure returned from a call to fname:vkGetBufferMemoryRequirements with pname:buffer
-- * The size of pname:buffer must: be less than or equal to the size of pname:memory minus pname:memoryOffset
-- * pname:memoryOffset must: be an integer multiple of the pname:alignment member of the sname:VkMemoryRequirements structure returned from a call to fname:vkGetBufferMemoryRequirements with pname:buffer
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkBindBufferMemory
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkBuffer
  -- ^ `buffer`  ExternSync=true.
  -> VkDeviceMemory
  -- ^ `memory` 
  -> VkDeviceSize
  -- ^ `memoryOffset` 
  -> m VkResult
vkBindBufferMemory vk device buffer memory memoryOffset =
  liftIO (ffi_vkBindBufferMemory (fp_vkBindBufferMemory vk) device buffer memory memoryOffset)


foreign import ccall unsafe "dynamic" ffi_vkGetImageMemoryRequirements :: FunPtr (VkDevice -> VkImage -> Ptr VkMemoryRequirements -> IO ()) -> (VkDevice -> VkImage -> Ptr VkMemoryRequirements -> IO ())

-- | @vkGetImageMemoryRequirements device image pMemoryRequirements@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetImageMemoryRequirements
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkImage
  -- ^ `image` 
  -> Ptr VkMemoryRequirements
  -- ^ `pMemoryRequirements` 
  -> m ()
vkGetImageMemoryRequirements vk device image pMemoryRequirements =
  liftIO (ffi_vkGetImageMemoryRequirements (fp_vkGetImageMemoryRequirements vk) device image pMemoryRequirements)


foreign import ccall unsafe "dynamic" ffi_vkBindImageMemory :: FunPtr (VkDevice -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO VkResult) -> (VkDevice -> VkImage -> VkDeviceMemory -> VkDeviceSize -> IO VkResult)

-- | @vkBindImageMemory device image memory memoryOffset@
-- 
-- == Validaty
-- * pname:image mustnot: already be backed by a memory object
-- * pname:image mustnot: have been created with any sparse memory binding flags
-- * pname:memoryOffset must: be less than the size of pname:memory
-- * pname:memory must: have been allocated using one of the memory types allowed in the pname:memoryTypeBits member of the sname:VkMemoryRequirements structure returned from a call to fname:vkGetImageMemoryRequirements with pname:image
-- * pname:memoryOffset must: be an integer multiple of the pname:alignment member of the sname:VkMemoryRequirements structure returned from a call to fname:vkGetImageMemoryRequirements with pname:image
-- * The pname:size member of the sname:VkMemoryRequirements structure returned from a call to fname:vkGetImageMemoryRequirements with pname:image must: be less than or equal to the size of pname:memory minus pname:memoryOffset
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkBindImageMemory
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkImage
  -- ^ `image`  ExternSync=true.
  -> VkDeviceMemory
  -- ^ `memory` 
  -> VkDeviceSize
  -- ^ `memoryOffset` 
  -> m VkResult
vkBindImageMemory vk device image memory memoryOffset =
  liftIO (ffi_vkBindImageMemory (fp_vkBindImageMemory vk) device image memory memoryOffset)


foreign import ccall unsafe "dynamic" ffi_vkGetImageSparseMemoryRequirements :: FunPtr (VkDevice -> VkImage -> Ptr Word32 -> Ptr VkSparseImageMemoryRequirements -> IO ()) -> (VkDevice -> VkImage -> Ptr Word32 -> Ptr VkSparseImageMemoryRequirements -> IO ())

-- | @vkGetImageSparseMemoryRequirements device image pSparseMemoryRequirementCount pSparseMemoryRequirements@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetImageSparseMemoryRequirements
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkImage
  -- ^ `image` 
  -> Ptr Word32
  -- ^ `pSparseMemoryRequirementCount`  Optional=false,true.
  -> Ptr VkSparseImageMemoryRequirements
  -- ^ `pSparseMemoryRequirements`  Can be `nullPtr`. Length: pSparseMemoryRequirementCount.
  -> m ()
vkGetImageSparseMemoryRequirements vk device image pSparseMemoryRequirementCount pSparseMemoryRequirements =
  liftIO (ffi_vkGetImageSparseMemoryRequirements (fp_vkGetImageSparseMemoryRequirements vk) device image pSparseMemoryRequirementCount pSparseMemoryRequirements)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceSparseImageFormatProperties :: FunPtr (VkPhysicalDevice -> VkFormat -> VkImageType -> VkSampleCountFlagBits -> VkImageUsageFlags -> VkImageTiling -> Ptr Word32 -> Ptr VkSparseImageFormatProperties -> IO ()) -> (VkPhysicalDevice -> VkFormat -> VkImageType -> VkSampleCountFlagBits -> VkImageUsageFlags -> VkImageTiling -> Ptr Word32 -> Ptr VkSparseImageFormatProperties -> IO ())

-- | @vkGetPhysicalDeviceSparseImageFormatProperties physicalDevice format imageType samples usage tiling pPropertyCount pProperties@
-- 
-- == Validaty
-- * If pname:format is an integer format, samples must: be one of the bit flags specified in sname:VkPhysicalDeviceLimits::pname:sampledImageIntegerSampleCounts
-- * If pname:format is a non-integer color format, samples must: be one of the bit flags specified in sname:VkPhysicalDeviceLimits::pname:sampledImageColorSampleCounts
-- * If pname:format is a depth format, samples must: be one of the bit flags specified in sname:VkPhysicalDeviceLimits::pname:sampledImageDepthSampleCounts
-- * If pname:format is a stencil format, samples must: be one of the bit flags specified in sname:VkPhysicalDeviceLimits::pname:sampledImageStencilSampleCounts
-- * If pname:usage includes ename:VK_IMAGE_USAGE_STORAGE_BIT, samples must: be one of the bit flags specified in sname:VkPhysicalDeviceLimits::pname:storageImageSampleCounts
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceSparseImageFormatProperties
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> VkFormat
  -- ^ `format` 
  -> VkImageType
  -- ^ `imageType` 
  -> VkSampleCountFlagBits
  -- ^ `samples` 
  -> VkImageUsageFlags
  -- ^ `usage` 
  -> VkImageTiling
  -- ^ `tiling` 
  -> Ptr Word32
  -- ^ `pPropertyCount`  Optional=false,true.
  -> Ptr VkSparseImageFormatProperties
  -- ^ `pProperties`  Can be `nullPtr`. Length: pPropertyCount.
  -> m ()
vkGetPhysicalDeviceSparseImageFormatProperties vk physicalDevice format imageType samples usage tiling pPropertyCount pProperties =
  liftIO (ffi_vkGetPhysicalDeviceSparseImageFormatProperties (fp_vkGetPhysicalDeviceSparseImageFormatProperties vk) physicalDevice format imageType samples usage tiling pPropertyCount pProperties)


foreign import ccall unsafe "dynamic" ffi_vkQueueBindSparse :: FunPtr (VkQueue -> Word32 -> Ptr VkBindSparseInfo -> VkFence -> IO VkResult) -> (VkQueue -> Word32 -> Ptr VkBindSparseInfo -> VkFence -> IO VkResult)

-- | @vkQueueBindSparse queue bindInfoCount pBindInfo fence@
-- 
-- == Validaty
-- * pname:fence must: be unsignalled
-- * pname:fence mustnot: be associated with any other queue command that has not yet completed execution on that queue
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkQueueBindSparse
  :: MonadIO m => Vulkan
  -> VkQueue
  -- ^ `queue`  ExternSync=true.
  -> Word32
  -- ^ `bindInfoCount`  Can be `nullPtr`.
  -> Ptr VkBindSparseInfo
  -- ^ `pBindInfo`  ExternSync=pBindInfo[].pWaitSemaphores[],pBindInfo[].pSignalSemaphores[],pBindInfo[].pBufferBinds[].buffer,pBindInfo[].pImageOpaqueBinds[].image,pBindInfo[].pImageBinds[].image. Const. Length: bindInfoCount.
  -> VkFence
  -- ^ `fence`  Can be `nullPtr`. ExternSync=true.
  -> m VkResult
vkQueueBindSparse vk queue bindInfoCount pBindInfo fence =
  liftIO (ffi_vkQueueBindSparse (fp_vkQueueBindSparse vk) queue bindInfoCount pBindInfo fence)


foreign import ccall unsafe "dynamic" ffi_vkCreateFence :: FunPtr (VkDevice -> Ptr VkFenceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult) -> (VkDevice -> Ptr VkFenceCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkFence -> IO VkResult)

-- | @vkCreateFence device pCreateInfo pAllocator pFence@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateFence
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkFenceCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkFence
  -- ^ `pFence` 
  -> m VkResult
vkCreateFence vk device pCreateInfo pAllocator pFence =
  liftIO (ffi_vkCreateFence (fp_vkCreateFence vk) device pCreateInfo pAllocator pFence)


foreign import ccall unsafe "dynamic" ffi_vkDestroyFence :: FunPtr (VkDevice -> VkFence -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkFence -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyFence device fence pAllocator@
-- 
-- == Validaty
-- * pname:fence mustnot: be associated with any queue command that has not yet completed execution on that queue
-- * If sname:VkAllocationCallbacks were provided when pname:fence was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:fence was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyFence
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkFence
  -- ^ `fence`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyFence vk device fence pAllocator =
  liftIO (ffi_vkDestroyFence (fp_vkDestroyFence vk) device fence pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkResetFences :: FunPtr (VkDevice -> Word32 -> Ptr VkFence -> IO VkResult) -> (VkDevice -> Word32 -> Ptr VkFence -> IO VkResult)

-- | @vkResetFences device fenceCount pFences@
-- 
-- == Validaty
-- * Any given element of pname:pFences mustnot: currently be associated with any queue command that has not yet completed execution on that queue
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkResetFences
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Word32
  -- ^ `fenceCount` 
  -> Ptr VkFence
  -- ^ `pFences`  ExternSync=true. Const. Length: fenceCount.
  -> m VkResult
vkResetFences vk device fenceCount pFences =
  liftIO (ffi_vkResetFences (fp_vkResetFences vk) device fenceCount pFences)


foreign import ccall unsafe "dynamic" ffi_vkGetFenceStatus :: FunPtr (VkDevice -> VkFence -> IO VkResult) -> (VkDevice -> VkFence -> IO VkResult)

-- | @vkGetFenceStatus device fence@
-- 
-- 
-- s:VK_SUCCESS,VK_NOT_READY e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST q: rp: cbl: iesp:[]
vkGetFenceStatus
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkFence
  -- ^ `fence` 
  -> m VkResult
vkGetFenceStatus vk device fence =
  liftIO (ffi_vkGetFenceStatus (fp_vkGetFenceStatus vk) device fence)


foreign import ccall unsafe "dynamic" ffi_vkWaitForFences :: FunPtr (VkDevice -> Word32 -> Ptr VkFence -> VkBool32 -> Word64 -> IO VkResult) -> (VkDevice -> Word32 -> Ptr VkFence -> VkBool32 -> Word64 -> IO VkResult)

-- | @vkWaitForFences device fenceCount pFences waitAll timeout@
-- 
-- 
-- s:VK_SUCCESS,VK_TIMEOUT e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST q: rp: cbl: iesp:[]
vkWaitForFences
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Word32
  -- ^ `fenceCount` 
  -> Ptr VkFence
  -- ^ `pFences`  Const. Length: fenceCount.
  -> VkBool32
  -- ^ `waitAll` 
  -> Word64
  -- ^ `timeout` 
  -> m VkResult
vkWaitForFences vk device fenceCount pFences waitAll timeout =
  liftIO (ffi_vkWaitForFences (fp_vkWaitForFences vk) device fenceCount pFences waitAll timeout)


foreign import ccall unsafe "dynamic" ffi_vkCreateSemaphore :: FunPtr (VkDevice -> Ptr VkSemaphoreCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkSemaphore -> IO VkResult) -> (VkDevice -> Ptr VkSemaphoreCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkSemaphore -> IO VkResult)

-- | @vkCreateSemaphore device pCreateInfo pAllocator pSemaphore@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateSemaphore
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkSemaphoreCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSemaphore
  -- ^ `pSemaphore` 
  -> m VkResult
vkCreateSemaphore vk device pCreateInfo pAllocator pSemaphore =
  liftIO (ffi_vkCreateSemaphore (fp_vkCreateSemaphore vk) device pCreateInfo pAllocator pSemaphore)


foreign import ccall unsafe "dynamic" ffi_vkDestroySemaphore :: FunPtr (VkDevice -> VkSemaphore -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkSemaphore -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroySemaphore device semaphore pAllocator@
-- 
-- == Validaty
-- * pname:semaphore mustnot: be associated with any queue command that has not yet completed execution on that queue
-- * If sname:VkAllocationCallbacks were provided when pname:semaphore was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:semaphore was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroySemaphore
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkSemaphore
  -- ^ `semaphore`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroySemaphore vk device semaphore pAllocator =
  liftIO (ffi_vkDestroySemaphore (fp_vkDestroySemaphore vk) device semaphore pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreateEvent :: FunPtr (VkDevice -> Ptr VkEventCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkEvent -> IO VkResult) -> (VkDevice -> Ptr VkEventCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkEvent -> IO VkResult)

-- | @vkCreateEvent device pCreateInfo pAllocator pEvent@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateEvent
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkEventCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkEvent
  -- ^ `pEvent` 
  -> m VkResult
vkCreateEvent vk device pCreateInfo pAllocator pEvent =
  liftIO (ffi_vkCreateEvent (fp_vkCreateEvent vk) device pCreateInfo pAllocator pEvent)


foreign import ccall unsafe "dynamic" ffi_vkDestroyEvent :: FunPtr (VkDevice -> VkEvent -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkEvent -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyEvent device event pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:event must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:event was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:event was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyEvent
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkEvent
  -- ^ `event`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyEvent vk device event pAllocator =
  liftIO (ffi_vkDestroyEvent (fp_vkDestroyEvent vk) device event pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkGetEventStatus :: FunPtr (VkDevice -> VkEvent -> IO VkResult) -> (VkDevice -> VkEvent -> IO VkResult)

-- | @vkGetEventStatus device event@
-- 
-- 
-- s:VK_EVENT_SET,VK_EVENT_RESET e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST q: rp: cbl: iesp:[]
vkGetEventStatus
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkEvent
  -- ^ `event` 
  -> m VkResult
vkGetEventStatus vk device event =
  liftIO (ffi_vkGetEventStatus (fp_vkGetEventStatus vk) device event)


foreign import ccall unsafe "dynamic" ffi_vkSetEvent :: FunPtr (VkDevice -> VkEvent -> IO VkResult) -> (VkDevice -> VkEvent -> IO VkResult)

-- | @vkSetEvent device event@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkSetEvent
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkEvent
  -- ^ `event`  ExternSync=true.
  -> m VkResult
vkSetEvent vk device event =
  liftIO (ffi_vkSetEvent (fp_vkSetEvent vk) device event)


foreign import ccall unsafe "dynamic" ffi_vkResetEvent :: FunPtr (VkDevice -> VkEvent -> IO VkResult) -> (VkDevice -> VkEvent -> IO VkResult)

-- | @vkResetEvent device event@
-- 
-- == Validaty
-- * pname:event mustnot: be waited on by a fname:vkCmdWaitEvents command that is currently executing
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkResetEvent
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkEvent
  -- ^ `event`  ExternSync=true.
  -> m VkResult
vkResetEvent vk device event =
  liftIO (ffi_vkResetEvent (fp_vkResetEvent vk) device event)


foreign import ccall unsafe "dynamic" ffi_vkCreateQueryPool :: FunPtr (VkDevice -> Ptr VkQueryPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkQueryPool -> IO VkResult) -> (VkDevice -> Ptr VkQueryPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkQueryPool -> IO VkResult)

-- | @vkCreateQueryPool device pCreateInfo pAllocator pQueryPool@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateQueryPool
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkQueryPoolCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkQueryPool
  -- ^ `pQueryPool` 
  -> m VkResult
vkCreateQueryPool vk device pCreateInfo pAllocator pQueryPool =
  liftIO (ffi_vkCreateQueryPool (fp_vkCreateQueryPool vk) device pCreateInfo pAllocator pQueryPool)


foreign import ccall unsafe "dynamic" ffi_vkDestroyQueryPool :: FunPtr (VkDevice -> VkQueryPool -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkQueryPool -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyQueryPool device queryPool pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:queryPool must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:queryPool was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:queryPool was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyQueryPool
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkQueryPool
  -- ^ `queryPool`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyQueryPool vk device queryPool pAllocator =
  liftIO (ffi_vkDestroyQueryPool (fp_vkDestroyQueryPool vk) device queryPool pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkGetQueryPoolResults :: FunPtr (VkDevice -> VkQueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> VkDeviceSize -> VkQueryResultFlags -> IO VkResult) -> (VkDevice -> VkQueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> VkDeviceSize -> VkQueryResultFlags -> IO VkResult)

-- | @vkGetQueryPoolResults device queryPool firstQuery queryCount dataSize pData stride flags@
-- 
-- == Validaty
-- * pname:firstQuery must: be less than the number of queries in pname:queryPool
-- * If ename:VK_QUERY_RESULT_64_BIT is not set in pname:flags then pname:pData and pname:stride must: be multiples of `4`
-- * If ename:VK_QUERY_RESULT_64_BIT is set in pname:flags then pname:pData and pname:stride must: be multiples of `8`
-- * The sum of pname:firstQuery and pname:queryCount must: be less than or equal to the number of queries in pname:queryPool
-- * pname:dataSize must: be large enough to contain the result of each query, as described <<queries-operation-memorylayout,here>>
-- * If the pname:queryType used to create pname:queryPool was ename:VK_QUERY_TYPE_TIMESTAMP, pname:flags mustnot: contain ename:VK_QUERY_RESULT_PARTIAL_BIT
-- 
-- s:VK_SUCCESS,VK_NOT_READY e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST q: rp: cbl: iesp:[]
vkGetQueryPoolResults
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkQueryPool
  -- ^ `queryPool` 
  -> Word32
  -- ^ `firstQuery` 
  -> Word32
  -- ^ `queryCount` 
  -> CSize
  -- ^ `dataSize` 
  -> Ptr ()
  -- ^ `pData`  Length: dataSize.
  -> VkDeviceSize
  -- ^ `stride` 
  -> VkQueryResultFlags
  -- ^ `flags`  Can be `nullPtr`.
  -> m VkResult
vkGetQueryPoolResults vk device queryPool firstQuery queryCount dataSize pData stride flags =
  liftIO (ffi_vkGetQueryPoolResults (fp_vkGetQueryPoolResults vk) device queryPool firstQuery queryCount dataSize pData stride flags)


foreign import ccall unsafe "dynamic" ffi_vkCreateBuffer :: FunPtr (VkDevice -> Ptr VkBufferCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkBuffer -> IO VkResult) -> (VkDevice -> Ptr VkBufferCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkBuffer -> IO VkResult)

-- | @vkCreateBuffer device pCreateInfo pAllocator pBuffer@
-- 
-- == Validaty
-- * If the pname:flags member of pname:pCreateInfo includes ename:VK_BUFFER_CREATE_SPARSE_BINDING_BIT, creating this sname:VkBuffer mustnot: cause the total required sparse memory for all currently valid sparse resources on the device to exceed sname:VkPhysicalDeviceLimits::pname:sparseAddressSpaceSize
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateBuffer
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkBufferCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkBuffer
  -- ^ `pBuffer` 
  -> m VkResult
vkCreateBuffer vk device pCreateInfo pAllocator pBuffer =
  liftIO (ffi_vkCreateBuffer (fp_vkCreateBuffer vk) device pCreateInfo pAllocator pBuffer)


foreign import ccall unsafe "dynamic" ffi_vkDestroyBuffer :: FunPtr (VkDevice -> VkBuffer -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkBuffer -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyBuffer device buffer pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:buffer, either directly or via a sname:VkBufferView, must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:buffer was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:buffer was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyBuffer
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkBuffer
  -- ^ `buffer`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyBuffer vk device buffer pAllocator =
  liftIO (ffi_vkDestroyBuffer (fp_vkDestroyBuffer vk) device buffer pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreateBufferView :: FunPtr (VkDevice -> Ptr VkBufferViewCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkBufferView -> IO VkResult) -> (VkDevice -> Ptr VkBufferViewCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkBufferView -> IO VkResult)

-- | @vkCreateBufferView device pCreateInfo pAllocator pView@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateBufferView
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkBufferViewCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkBufferView
  -- ^ `pView` 
  -> m VkResult
vkCreateBufferView vk device pCreateInfo pAllocator pView =
  liftIO (ffi_vkCreateBufferView (fp_vkCreateBufferView vk) device pCreateInfo pAllocator pView)


foreign import ccall unsafe "dynamic" ffi_vkDestroyBufferView :: FunPtr (VkDevice -> VkBufferView -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkBufferView -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyBufferView device bufferView pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:bufferView must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:bufferView was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:bufferView was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyBufferView
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkBufferView
  -- ^ `bufferView`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyBufferView vk device bufferView pAllocator =
  liftIO (ffi_vkDestroyBufferView (fp_vkDestroyBufferView vk) device bufferView pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreateImage :: FunPtr (VkDevice -> Ptr VkImageCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImage -> IO VkResult) -> (VkDevice -> Ptr VkImageCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImage -> IO VkResult)

-- | @vkCreateImage device pCreateInfo pAllocator pImage@
-- 
-- == Validaty
-- * If the pname:flags member of pname:pCreateInfo includes ename:VK_IMAGE_CREATE_SPARSE_BINDING_BIT, creating this sname:VkImage mustnot: cause the total required sparse memory for all currently valid sparse resources on the device to exceed sname:VkPhysicalDeviceLimits::pname:sparseAddressSpaceSize
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateImage
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkImageCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkImage
  -- ^ `pImage` 
  -> m VkResult
vkCreateImage vk device pCreateInfo pAllocator pImage =
  liftIO (ffi_vkCreateImage (fp_vkCreateImage vk) device pCreateInfo pAllocator pImage)


foreign import ccall unsafe "dynamic" ffi_vkDestroyImage :: FunPtr (VkDevice -> VkImage -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkImage -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyImage device image pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:image, either directly or via a sname:VkImageView, must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:image was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:image was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyImage
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkImage
  -- ^ `image`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyImage vk device image pAllocator =
  liftIO (ffi_vkDestroyImage (fp_vkDestroyImage vk) device image pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkGetImageSubresourceLayout :: FunPtr (VkDevice -> VkImage -> Ptr VkImageSubresource -> Ptr VkSubresourceLayout -> IO ()) -> (VkDevice -> VkImage -> Ptr VkImageSubresource -> Ptr VkSubresourceLayout -> IO ())

-- | @vkGetImageSubresourceLayout device image pSubresource pLayout@
-- 
-- == Validaty
-- * pname:image must: have been created with pname:tiling equal to ename:VK_IMAGE_TILING_LINEAR
-- * The pname:aspectMask member of pname:pSubresource must: only have a single bit set
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetImageSubresourceLayout
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkImage
  -- ^ `image` 
  -> Ptr VkImageSubresource
  -- ^ `pSubresource`  Const.
  -> Ptr VkSubresourceLayout
  -- ^ `pLayout` 
  -> m ()
vkGetImageSubresourceLayout vk device image pSubresource pLayout =
  liftIO (ffi_vkGetImageSubresourceLayout (fp_vkGetImageSubresourceLayout vk) device image pSubresource pLayout)


foreign import ccall unsafe "dynamic" ffi_vkCreateImageView :: FunPtr (VkDevice -> Ptr VkImageViewCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImageView -> IO VkResult) -> (VkDevice -> Ptr VkImageViewCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkImageView -> IO VkResult)

-- | @vkCreateImageView device pCreateInfo pAllocator pView@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateImageView
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkImageViewCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkImageView
  -- ^ `pView` 
  -> m VkResult
vkCreateImageView vk device pCreateInfo pAllocator pView =
  liftIO (ffi_vkCreateImageView (fp_vkCreateImageView vk) device pCreateInfo pAllocator pView)


foreign import ccall unsafe "dynamic" ffi_vkDestroyImageView :: FunPtr (VkDevice -> VkImageView -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkImageView -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyImageView device imageView pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:imageView must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:imageView was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:imageView was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyImageView
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkImageView
  -- ^ `imageView`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyImageView vk device imageView pAllocator =
  liftIO (ffi_vkDestroyImageView (fp_vkDestroyImageView vk) device imageView pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreateShaderModule :: FunPtr (VkDevice -> Ptr VkShaderModuleCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkShaderModule -> IO VkResult) -> (VkDevice -> Ptr VkShaderModuleCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkShaderModule -> IO VkResult)

-- | @vkCreateShaderModule device pCreateInfo pAllocator pShaderModule@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateShaderModule
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkShaderModuleCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkShaderModule
  -- ^ `pShaderModule` 
  -> m VkResult
vkCreateShaderModule vk device pCreateInfo pAllocator pShaderModule =
  liftIO (ffi_vkCreateShaderModule (fp_vkCreateShaderModule vk) device pCreateInfo pAllocator pShaderModule)


foreign import ccall unsafe "dynamic" ffi_vkDestroyShaderModule :: FunPtr (VkDevice -> VkShaderModule -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkShaderModule -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyShaderModule device shaderModule pAllocator@
-- 
-- == Validaty
-- * If sname:VkAllocationCallbacks were provided when pname:shaderModule was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:shaderModule was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyShaderModule
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkShaderModule
  -- ^ `shaderModule`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyShaderModule vk device shaderModule pAllocator =
  liftIO (ffi_vkDestroyShaderModule (fp_vkDestroyShaderModule vk) device shaderModule pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreatePipelineCache :: FunPtr (VkDevice -> Ptr VkPipelineCacheCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipelineCache -> IO VkResult) -> (VkDevice -> Ptr VkPipelineCacheCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipelineCache -> IO VkResult)

-- | @vkCreatePipelineCache device pCreateInfo pAllocator pPipelineCache@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreatePipelineCache
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkPipelineCacheCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkPipelineCache
  -- ^ `pPipelineCache` 
  -> m VkResult
vkCreatePipelineCache vk device pCreateInfo pAllocator pPipelineCache =
  liftIO (ffi_vkCreatePipelineCache (fp_vkCreatePipelineCache vk) device pCreateInfo pAllocator pPipelineCache)


foreign import ccall unsafe "dynamic" ffi_vkDestroyPipelineCache :: FunPtr (VkDevice -> VkPipelineCache -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkPipelineCache -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyPipelineCache device pipelineCache pAllocator@
-- 
-- == Validaty
-- * If sname:VkAllocationCallbacks were provided when pname:pipelineCache was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:pipelineCache was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyPipelineCache
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkPipelineCache
  -- ^ `pipelineCache`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyPipelineCache vk device pipelineCache pAllocator =
  liftIO (ffi_vkDestroyPipelineCache (fp_vkDestroyPipelineCache vk) device pipelineCache pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkGetPipelineCacheData :: FunPtr (VkDevice -> VkPipelineCache -> Ptr CSize -> Ptr () -> IO VkResult) -> (VkDevice -> VkPipelineCache -> Ptr CSize -> Ptr () -> IO VkResult)

-- | @vkGetPipelineCacheData device pipelineCache pDataSize pData@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkGetPipelineCacheData
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkPipelineCache
  -- ^ `pipelineCache` 
  -> Ptr CSize
  -- ^ `pDataSize`  Optional=false,true.
  -> Ptr ()
  -- ^ `pData`  Can be `nullPtr`. Length: pDataSize.
  -> m VkResult
vkGetPipelineCacheData vk device pipelineCache pDataSize pData =
  liftIO (ffi_vkGetPipelineCacheData (fp_vkGetPipelineCacheData vk) device pipelineCache pDataSize pData)


foreign import ccall unsafe "dynamic" ffi_vkMergePipelineCaches :: FunPtr (VkDevice -> VkPipelineCache -> Word32 -> Ptr VkPipelineCache -> IO VkResult) -> (VkDevice -> VkPipelineCache -> Word32 -> Ptr VkPipelineCache -> IO VkResult)

-- | @vkMergePipelineCaches device dstCache srcCacheCount pSrcCaches@
-- 
-- == Validaty
-- * pname:dstCache mustnot: appear in the list of source caches
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkMergePipelineCaches
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkPipelineCache
  -- ^ `dstCache`  ExternSync=true.
  -> Word32
  -- ^ `srcCacheCount` 
  -> Ptr VkPipelineCache
  -- ^ `pSrcCaches`  Const. Length: srcCacheCount.
  -> m VkResult
vkMergePipelineCaches vk device dstCache srcCacheCount pSrcCaches =
  liftIO (ffi_vkMergePipelineCaches (fp_vkMergePipelineCaches vk) device dstCache srcCacheCount pSrcCaches)


foreign import ccall unsafe "dynamic" ffi_vkCreateGraphicsPipelines :: FunPtr (VkDevice -> VkPipelineCache -> Word32 -> Ptr VkGraphicsPipelineCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult) -> (VkDevice -> VkPipelineCache -> Word32 -> Ptr VkGraphicsPipelineCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult)

-- | @vkCreateGraphicsPipelines device pipelineCache createInfoCount pCreateInfos pAllocator pPipelines@
-- 
-- == Validaty
-- * If the pname:flags member of any given element of pname:pCreateInfos contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and the pname:basePipelineIndex member of that same element is not `-1`, pname:basePipelineIndex must: be less than the index into pname:pCreateInfos that corresponds to that element
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateGraphicsPipelines
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkPipelineCache
  -- ^ `pipelineCache`  Can be `nullPtr`.
  -> Word32
  -- ^ `createInfoCount` 
  -> Ptr VkGraphicsPipelineCreateInfo
  -- ^ `pCreateInfos`  Const. Length: createInfoCount.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkPipeline
  -- ^ `pPipelines`  Length: createInfoCount.
  -> m VkResult
vkCreateGraphicsPipelines vk device pipelineCache createInfoCount pCreateInfos pAllocator pPipelines =
  liftIO (ffi_vkCreateGraphicsPipelines (fp_vkCreateGraphicsPipelines vk) device pipelineCache createInfoCount pCreateInfos pAllocator pPipelines)


foreign import ccall unsafe "dynamic" ffi_vkCreateComputePipelines :: FunPtr (VkDevice -> VkPipelineCache -> Word32 -> Ptr VkComputePipelineCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult) -> (VkDevice -> VkPipelineCache -> Word32 -> Ptr VkComputePipelineCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipeline -> IO VkResult)

-- | @vkCreateComputePipelines device pipelineCache createInfoCount pCreateInfos pAllocator pPipelines@
-- 
-- == Validaty
-- * If the pname:flags member of any given element of pname:pCreateInfos contains the ename:VK_PIPELINE_CREATE_DERIVATIVE_BIT flag, and the pname:basePipelineIndex member of that same element is not `-1`, pname:basePipelineIndex must: be less than the index into pname:pCreateInfos that corresponds to that element
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateComputePipelines
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkPipelineCache
  -- ^ `pipelineCache`  Can be `nullPtr`.
  -> Word32
  -- ^ `createInfoCount` 
  -> Ptr VkComputePipelineCreateInfo
  -- ^ `pCreateInfos`  Const. Length: createInfoCount.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkPipeline
  -- ^ `pPipelines`  Length: createInfoCount.
  -> m VkResult
vkCreateComputePipelines vk device pipelineCache createInfoCount pCreateInfos pAllocator pPipelines =
  liftIO (ffi_vkCreateComputePipelines (fp_vkCreateComputePipelines vk) device pipelineCache createInfoCount pCreateInfos pAllocator pPipelines)


foreign import ccall unsafe "dynamic" ffi_vkDestroyPipeline :: FunPtr (VkDevice -> VkPipeline -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkPipeline -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyPipeline device pipeline pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:pipeline must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:pipeline was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:pipeline was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyPipeline
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkPipeline
  -- ^ `pipeline`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyPipeline vk device pipeline pAllocator =
  liftIO (ffi_vkDestroyPipeline (fp_vkDestroyPipeline vk) device pipeline pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreatePipelineLayout :: FunPtr (VkDevice -> Ptr VkPipelineLayoutCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipelineLayout -> IO VkResult) -> (VkDevice -> Ptr VkPipelineLayoutCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkPipelineLayout -> IO VkResult)

-- | @vkCreatePipelineLayout device pCreateInfo pAllocator pPipelineLayout@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreatePipelineLayout
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkPipelineLayoutCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkPipelineLayout
  -- ^ `pPipelineLayout` 
  -> m VkResult
vkCreatePipelineLayout vk device pCreateInfo pAllocator pPipelineLayout =
  liftIO (ffi_vkCreatePipelineLayout (fp_vkCreatePipelineLayout vk) device pCreateInfo pAllocator pPipelineLayout)


foreign import ccall unsafe "dynamic" ffi_vkDestroyPipelineLayout :: FunPtr (VkDevice -> VkPipelineLayout -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkPipelineLayout -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyPipelineLayout device pipelineLayout pAllocator@
-- 
-- == Validaty
-- * If sname:VkAllocationCallbacks were provided when pname:pipelineLayout was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:pipelineLayout was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyPipelineLayout
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkPipelineLayout
  -- ^ `pipelineLayout`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyPipelineLayout vk device pipelineLayout pAllocator =
  liftIO (ffi_vkDestroyPipelineLayout (fp_vkDestroyPipelineLayout vk) device pipelineLayout pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreateSampler :: FunPtr (VkDevice -> Ptr VkSamplerCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkSampler -> IO VkResult) -> (VkDevice -> Ptr VkSamplerCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkSampler -> IO VkResult)

-- | @vkCreateSampler device pCreateInfo pAllocator pSampler@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_TOO_MANY_OBJECTS q: rp: cbl: iesp:[]
vkCreateSampler
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkSamplerCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSampler
  -- ^ `pSampler` 
  -> m VkResult
vkCreateSampler vk device pCreateInfo pAllocator pSampler =
  liftIO (ffi_vkCreateSampler (fp_vkCreateSampler vk) device pCreateInfo pAllocator pSampler)


foreign import ccall unsafe "dynamic" ffi_vkDestroySampler :: FunPtr (VkDevice -> VkSampler -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkSampler -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroySampler device sampler pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:sampler must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:sampler was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:sampler was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroySampler
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkSampler
  -- ^ `sampler`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroySampler vk device sampler pAllocator =
  liftIO (ffi_vkDestroySampler (fp_vkDestroySampler vk) device sampler pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreateDescriptorSetLayout :: FunPtr (VkDevice -> Ptr VkDescriptorSetLayoutCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDescriptorSetLayout -> IO VkResult) -> (VkDevice -> Ptr VkDescriptorSetLayoutCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDescriptorSetLayout -> IO VkResult)

-- | @vkCreateDescriptorSetLayout device pCreateInfo pAllocator pSetLayout@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateDescriptorSetLayout
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkDescriptorSetLayoutCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkDescriptorSetLayout
  -- ^ `pSetLayout` 
  -> m VkResult
vkCreateDescriptorSetLayout vk device pCreateInfo pAllocator pSetLayout =
  liftIO (ffi_vkCreateDescriptorSetLayout (fp_vkCreateDescriptorSetLayout vk) device pCreateInfo pAllocator pSetLayout)


foreign import ccall unsafe "dynamic" ffi_vkDestroyDescriptorSetLayout :: FunPtr (VkDevice -> VkDescriptorSetLayout -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkDescriptorSetLayout -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyDescriptorSetLayout device descriptorSetLayout pAllocator@
-- 
-- == Validaty
-- * If sname:VkAllocationCallbacks were provided when pname:descriptorSetLayout was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:descriptorSetLayout was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyDescriptorSetLayout
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkDescriptorSetLayout
  -- ^ `descriptorSetLayout`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyDescriptorSetLayout vk device descriptorSetLayout pAllocator =
  liftIO (ffi_vkDestroyDescriptorSetLayout (fp_vkDestroyDescriptorSetLayout vk) device descriptorSetLayout pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreateDescriptorPool :: FunPtr (VkDevice -> Ptr VkDescriptorPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDescriptorPool -> IO VkResult) -> (VkDevice -> Ptr VkDescriptorPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkDescriptorPool -> IO VkResult)

-- | @vkCreateDescriptorPool device pCreateInfo pAllocator pDescriptorPool@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateDescriptorPool
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkDescriptorPoolCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkDescriptorPool
  -- ^ `pDescriptorPool` 
  -> m VkResult
vkCreateDescriptorPool vk device pCreateInfo pAllocator pDescriptorPool =
  liftIO (ffi_vkCreateDescriptorPool (fp_vkCreateDescriptorPool vk) device pCreateInfo pAllocator pDescriptorPool)


foreign import ccall unsafe "dynamic" ffi_vkDestroyDescriptorPool :: FunPtr (VkDevice -> VkDescriptorPool -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkDescriptorPool -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyDescriptorPool device descriptorPool pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:descriptorPool (via any allocated descriptor sets) must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:descriptorPool was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:descriptorPool was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyDescriptorPool
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkDescriptorPool
  -- ^ `descriptorPool`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyDescriptorPool vk device descriptorPool pAllocator =
  liftIO (ffi_vkDestroyDescriptorPool (fp_vkDestroyDescriptorPool vk) device descriptorPool pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkResetDescriptorPool :: FunPtr (VkDevice -> VkDescriptorPool -> VkDescriptorPoolResetFlags -> IO VkResult) -> (VkDevice -> VkDescriptorPool -> VkDescriptorPoolResetFlags -> IO VkResult)

-- | @vkResetDescriptorPool device descriptorPool flags@
-- 
-- == Validaty
-- * All uses of pname:descriptorPool (via any allocated descriptor sets) must: have completed execution
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkResetDescriptorPool
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkDescriptorPool
  -- ^ `descriptorPool`  ExternSync=true.
  -> VkDescriptorPoolResetFlags
  -- ^ `flags`  Can be `nullPtr`.
  -> m VkResult
vkResetDescriptorPool vk device descriptorPool flags =
  liftIO (ffi_vkResetDescriptorPool (fp_vkResetDescriptorPool vk) device descriptorPool flags)


foreign import ccall unsafe "dynamic" ffi_vkAllocateDescriptorSets :: FunPtr (VkDevice -> Ptr VkDescriptorSetAllocateInfo -> Ptr VkDescriptorSet -> IO VkResult) -> (VkDevice -> Ptr VkDescriptorSetAllocateInfo -> Ptr VkDescriptorSet -> IO VkResult)

-- | @vkAllocateDescriptorSets device pAllocateInfo pDescriptorSets@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkAllocateDescriptorSets
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkDescriptorSetAllocateInfo
  -- ^ `pAllocateInfo`  ExternSync=pAllocateInfo->descriptorPool. Const.
  -> Ptr VkDescriptorSet
  -- ^ `pDescriptorSets`  Length: pAllocateInfo->descriptorSetCount.
  -> m VkResult
vkAllocateDescriptorSets vk device pAllocateInfo pDescriptorSets =
  liftIO (ffi_vkAllocateDescriptorSets (fp_vkAllocateDescriptorSets vk) device pAllocateInfo pDescriptorSets)


foreign import ccall unsafe "dynamic" ffi_vkFreeDescriptorSets :: FunPtr (VkDevice -> VkDescriptorPool -> Word32 -> Ptr VkDescriptorSet -> IO VkResult) -> (VkDevice -> VkDescriptorPool -> Word32 -> Ptr VkDescriptorSet -> IO VkResult)

-- | @vkFreeDescriptorSets device descriptorPool descriptorSetCount pDescriptorSets@
-- 
-- == Validaty
-- * All submitted commands that refer to any element of pname:pDescriptorSets must: have completed execution
-- * pname:pDescriptorSets must: be a pointer to an array of pname:descriptorSetCount sname:VkDescriptorSet handles, each element of which must: either be a valid handle or sname:VK_NULL_HANDLE
-- * pname:descriptorPool must: have been created with the ename:VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT flag
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkFreeDescriptorSets
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkDescriptorPool
  -- ^ `descriptorPool`  ExternSync=true.
  -> Word32
  -- ^ `descriptorSetCount` 
  -> Ptr VkDescriptorSet
  -- ^ `pDescriptorSets`  ExternSync=true. Const. Length: descriptorSetCount. Noautovalidity.
  -> m VkResult
vkFreeDescriptorSets vk device descriptorPool descriptorSetCount pDescriptorSets =
  liftIO (ffi_vkFreeDescriptorSets (fp_vkFreeDescriptorSets vk) device descriptorPool descriptorSetCount pDescriptorSets)


foreign import ccall unsafe "dynamic" ffi_vkUpdateDescriptorSets :: FunPtr (VkDevice -> Word32 -> Ptr VkWriteDescriptorSet -> Word32 -> Ptr VkCopyDescriptorSet -> IO ()) -> (VkDevice -> Word32 -> Ptr VkWriteDescriptorSet -> Word32 -> Ptr VkCopyDescriptorSet -> IO ())

-- | @vkUpdateDescriptorSets device descriptorWriteCount pDescriptorWrites descriptorCopyCount pDescriptorCopies@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkUpdateDescriptorSets
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Word32
  -- ^ `descriptorWriteCount`  Can be `nullPtr`.
  -> Ptr VkWriteDescriptorSet
  -- ^ `pDescriptorWrites`  ExternSync=pDescriptorWrites[].dstSet. Const. Length: descriptorWriteCount.
  -> Word32
  -- ^ `descriptorCopyCount`  Can be `nullPtr`.
  -> Ptr VkCopyDescriptorSet
  -- ^ `pDescriptorCopies`  ExternSync=pDescriptorCopies[].dstSet. Const. Length: descriptorCopyCount.
  -> m ()
vkUpdateDescriptorSets vk device descriptorWriteCount pDescriptorWrites descriptorCopyCount pDescriptorCopies =
  liftIO (ffi_vkUpdateDescriptorSets (fp_vkUpdateDescriptorSets vk) device descriptorWriteCount pDescriptorWrites descriptorCopyCount pDescriptorCopies)


foreign import ccall unsafe "dynamic" ffi_vkCreateFramebuffer :: FunPtr (VkDevice -> Ptr VkFramebufferCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkFramebuffer -> IO VkResult) -> (VkDevice -> Ptr VkFramebufferCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkFramebuffer -> IO VkResult)

-- | @vkCreateFramebuffer device pCreateInfo pAllocator pFramebuffer@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateFramebuffer
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkFramebufferCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkFramebuffer
  -- ^ `pFramebuffer` 
  -> m VkResult
vkCreateFramebuffer vk device pCreateInfo pAllocator pFramebuffer =
  liftIO (ffi_vkCreateFramebuffer (fp_vkCreateFramebuffer vk) device pCreateInfo pAllocator pFramebuffer)


foreign import ccall unsafe "dynamic" ffi_vkDestroyFramebuffer :: FunPtr (VkDevice -> VkFramebuffer -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkFramebuffer -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyFramebuffer device framebuffer pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:framebuffer must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:framebuffer was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:framebuffer was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyFramebuffer
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkFramebuffer
  -- ^ `framebuffer`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyFramebuffer vk device framebuffer pAllocator =
  liftIO (ffi_vkDestroyFramebuffer (fp_vkDestroyFramebuffer vk) device framebuffer pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkCreateRenderPass :: FunPtr (VkDevice -> Ptr VkRenderPassCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkRenderPass -> IO VkResult) -> (VkDevice -> Ptr VkRenderPassCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkRenderPass -> IO VkResult)

-- | @vkCreateRenderPass device pCreateInfo pAllocator pRenderPass@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateRenderPass
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkRenderPassCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkRenderPass
  -- ^ `pRenderPass` 
  -> m VkResult
vkCreateRenderPass vk device pCreateInfo pAllocator pRenderPass =
  liftIO (ffi_vkCreateRenderPass (fp_vkCreateRenderPass vk) device pCreateInfo pAllocator pRenderPass)


foreign import ccall unsafe "dynamic" ffi_vkDestroyRenderPass :: FunPtr (VkDevice -> VkRenderPass -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkRenderPass -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyRenderPass device renderPass pAllocator@
-- 
-- == Validaty
-- * All submitted commands that refer to pname:renderPass must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:renderPass was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:renderPass was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyRenderPass
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkRenderPass
  -- ^ `renderPass`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyRenderPass vk device renderPass pAllocator =
  liftIO (ffi_vkDestroyRenderPass (fp_vkDestroyRenderPass vk) device renderPass pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkGetRenderAreaGranularity :: FunPtr (VkDevice -> VkRenderPass -> Ptr VkExtent2D -> IO ()) -> (VkDevice -> VkRenderPass -> Ptr VkExtent2D -> IO ())

-- | @vkGetRenderAreaGranularity device renderPass pGranularity@
-- 
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetRenderAreaGranularity
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkRenderPass
  -- ^ `renderPass` 
  -> Ptr VkExtent2D
  -- ^ `pGranularity` 
  -> m ()
vkGetRenderAreaGranularity vk device renderPass pGranularity =
  liftIO (ffi_vkGetRenderAreaGranularity (fp_vkGetRenderAreaGranularity vk) device renderPass pGranularity)


foreign import ccall unsafe "dynamic" ffi_vkCreateCommandPool :: FunPtr (VkDevice -> Ptr VkCommandPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkCommandPool -> IO VkResult) -> (VkDevice -> Ptr VkCommandPoolCreateInfo -> Ptr VkAllocationCallbacks -> Ptr VkCommandPool -> IO VkResult)

-- | @vkCreateCommandPool device pCreateInfo pAllocator pCommandPool@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateCommandPool
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkCommandPoolCreateInfo
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkCommandPool
  -- ^ `pCommandPool` 
  -> m VkResult
vkCreateCommandPool vk device pCreateInfo pAllocator pCommandPool =
  liftIO (ffi_vkCreateCommandPool (fp_vkCreateCommandPool vk) device pCreateInfo pAllocator pCommandPool)


foreign import ccall unsafe "dynamic" ffi_vkDestroyCommandPool :: FunPtr (VkDevice -> VkCommandPool -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkCommandPool -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyCommandPool device commandPool pAllocator@
-- 
-- == Validaty
-- * All sname:VkCommandBuffer objects allocated from pname:commandPool mustnot: be pending execution
-- * If sname:VkAllocationCallbacks were provided when pname:commandPool was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:commandPool was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyCommandPool
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkCommandPool
  -- ^ `commandPool`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyCommandPool vk device commandPool pAllocator =
  liftIO (ffi_vkDestroyCommandPool (fp_vkDestroyCommandPool vk) device commandPool pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkResetCommandPool :: FunPtr (VkDevice -> VkCommandPool -> VkCommandPoolResetFlags -> IO VkResult) -> (VkDevice -> VkCommandPool -> VkCommandPoolResetFlags -> IO VkResult)

-- | @vkResetCommandPool device commandPool flags@
-- 
-- == Validaty
-- * All sname:VkCommandBuffer objects allocated from pname:commandPool mustnot: currently be pending execution
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkResetCommandPool
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkCommandPool
  -- ^ `commandPool`  ExternSync=true.
  -> VkCommandPoolResetFlags
  -- ^ `flags`  Can be `nullPtr`.
  -> m VkResult
vkResetCommandPool vk device commandPool flags =
  liftIO (ffi_vkResetCommandPool (fp_vkResetCommandPool vk) device commandPool flags)


foreign import ccall unsafe "dynamic" ffi_vkAllocateCommandBuffers :: FunPtr (VkDevice -> Ptr VkCommandBufferAllocateInfo -> Ptr VkCommandBuffer -> IO VkResult) -> (VkDevice -> Ptr VkCommandBufferAllocateInfo -> Ptr VkCommandBuffer -> IO VkResult)

-- | @vkAllocateCommandBuffers device pAllocateInfo pCommandBuffers@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkAllocateCommandBuffers
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkCommandBufferAllocateInfo
  -- ^ `pAllocateInfo`  ExternSync=pAllocateInfo->commandPool. Const.
  -> Ptr VkCommandBuffer
  -- ^ `pCommandBuffers`  Length: pAllocateInfo->commandBufferCount.
  -> m VkResult
vkAllocateCommandBuffers vk device pAllocateInfo pCommandBuffers =
  liftIO (ffi_vkAllocateCommandBuffers (fp_vkAllocateCommandBuffers vk) device pAllocateInfo pCommandBuffers)


foreign import ccall unsafe "dynamic" ffi_vkFreeCommandBuffers :: FunPtr (VkDevice -> VkCommandPool -> Word32 -> Ptr VkCommandBuffer -> IO ()) -> (VkDevice -> VkCommandPool -> Word32 -> Ptr VkCommandBuffer -> IO ())

-- | @vkFreeCommandBuffers device commandPool commandBufferCount pCommandBuffers@
-- 
-- == Validaty
-- * All elements of pname:pCommandBuffers mustnot: be pending execution
-- * pname:pCommandBuffers must: be a pointer to an array of pname:commandBufferCount sname:VkCommandBuffer handles, each element of which must: either be a valid handle or sname:VK_NULL_HANDLE
-- 
-- s: e: q: rp: cbl: iesp:[]
vkFreeCommandBuffers
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkCommandPool
  -- ^ `commandPool`  ExternSync=true.
  -> Word32
  -- ^ `commandBufferCount` 
  -> Ptr VkCommandBuffer
  -- ^ `pCommandBuffers`  ExternSync=true. Const. Length: commandBufferCount. Noautovalidity.
  -> m ()
vkFreeCommandBuffers vk device commandPool commandBufferCount pCommandBuffers =
  liftIO (ffi_vkFreeCommandBuffers (fp_vkFreeCommandBuffers vk) device commandPool commandBufferCount pCommandBuffers)


foreign import ccall unsafe "dynamic" ffi_vkBeginCommandBuffer :: FunPtr (VkCommandBuffer -> Ptr VkCommandBufferBeginInfo -> IO VkResult) -> (VkCommandBuffer -> Ptr VkCommandBufferBeginInfo -> IO VkResult)

-- | @vkBeginCommandBuffer commandBuffer pBeginInfo@
-- 
-- == Validaty
-- * pname:commandBuffer mustnot: be in the recording state
-- * pname:commandBuffer mustnot: currently be pending execution
-- * If pname:commandBuffer was allocated from a sname:VkCommandPool which did not have the ename:VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT flag set, pname:commandBuffer must: be in the initial state.
-- * If pname:commandBuffer is a secondary command buffer, the pname:pInheritanceInfo member of pname:pBeginInfo must: be a valid sname:VkCommandBufferInheritanceInfo structure
-- * If pname:commandBuffer is a secondary command buffer and either the pname:occlusionQueryEnable member of the pname:pInheritanceInfo member of pname:pBeginInfo is ename:VK_FALSE, or the precise occlusion queries feature is not enabled, the pname:queryFlags member of the pname:pInheritanceInfo member pname:pBeginInfo mustnot: contain ename:VK_QUERY_CONTROL_PRECISE_BIT
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkBeginCommandBuffer
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Ptr VkCommandBufferBeginInfo
  -- ^ `pBeginInfo`  Const.
  -> m VkResult
vkBeginCommandBuffer vk commandBuffer pBeginInfo =
  liftIO (ffi_vkBeginCommandBuffer (fp_vkBeginCommandBuffer vk) commandBuffer pBeginInfo)


foreign import ccall unsafe "dynamic" ffi_vkEndCommandBuffer :: FunPtr (VkCommandBuffer -> IO VkResult) -> (VkCommandBuffer -> IO VkResult)

-- | @vkEndCommandBuffer commandBuffer@
-- 
-- == Validaty
-- * pname:commandBuffer must: be in the recording state
-- * fname:vkEndCommandBuffer mustnot: be called inside a render pass instance
-- * All queries made <<queries-operation-active,active>> during the recording of pname:commandBuffer must: have been made inactive
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkEndCommandBuffer
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> m VkResult
vkEndCommandBuffer vk commandBuffer =
  liftIO (ffi_vkEndCommandBuffer (fp_vkEndCommandBuffer vk) commandBuffer)


foreign import ccall unsafe "dynamic" ffi_vkResetCommandBuffer :: FunPtr (VkCommandBuffer -> VkCommandBufferResetFlags -> IO VkResult) -> (VkCommandBuffer -> VkCommandBufferResetFlags -> IO VkResult)

-- | @vkResetCommandBuffer commandBuffer flags@
-- 
-- == Validaty
-- * pname:commandBuffer mustnot: currently be pending execution
-- * pname:commandBuffer must: have been allocated from a pool that was created with the ename:VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkResetCommandBuffer
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkCommandBufferResetFlags
  -- ^ `flags`  Can be `nullPtr`.
  -> m VkResult
vkResetCommandBuffer vk commandBuffer flags =
  liftIO (ffi_vkResetCommandBuffer (fp_vkResetCommandBuffer vk) commandBuffer flags)


foreign import ccall unsafe "dynamic" ffi_vkCmdBindPipeline :: FunPtr (VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> IO ()) -> (VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> IO ())

-- | @vkCmdBindPipeline commandBuffer pipelineBindPoint pipeline@
-- 
-- == Validaty
-- * If pname:pipelineBindPoint is ename:VK_PIPELINE_BIND_POINT_COMPUTE, the sname:VkCommandPool that pname:commandBuffer was allocated from must: support compute operations
-- * If pname:pipelineBindPoint is ename:VK_PIPELINE_BIND_POINT_GRAPHICS, the sname:VkCommandPool that pname:commandBuffer was allocated from must: support graphics operations
-- * If pname:pipelineBindPoint is ename:VK_PIPELINE_BIND_POINT_COMPUTE, pname:pipeline must: be a compute pipeline
-- * If pname:pipelineBindPoint is ename:VK_PIPELINE_BIND_POINT_GRAPHICS, pname:pipeline must: be a graphics pipeline
-- * If the <<features-features-variableMultisampleRate,variable multisample rate>> feature is not supported, pname:pipeline is a graphics pipeline, the current subpass has no attachments, and this is not the first call to this function with a graphics pipeline after transitioning to the current subpass, then the sample count specified by this pipeline must: match that set in the previous pipeline
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdBindPipeline
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkPipelineBindPoint
  -- ^ `pipelineBindPoint` 
  -> VkPipeline
  -- ^ `pipeline` 
  -> m ()
vkCmdBindPipeline vk commandBuffer pipelineBindPoint pipeline =
  liftIO (ffi_vkCmdBindPipeline (fp_vkCmdBindPipeline vk) commandBuffer pipelineBindPoint pipeline)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetViewport :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkViewport -> IO ()) -> (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkViewport -> IO ())

-- | @vkCmdSetViewport commandBuffer firstViewport viewportCount pViewports@
-- 
-- == Validaty
-- * pname:firstViewport must: be less than sname:VkPhysicalDeviceLimits::pname:maxViewports
-- * The sum of pname:firstViewport and pname:viewportCount must: be between `1` and sname:VkPhysicalDeviceLimits::pname:maxViewports, inclusive
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdSetViewport
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Word32
  -- ^ `firstViewport` 
  -> Word32
  -- ^ `viewportCount` 
  -> Ptr VkViewport
  -- ^ `pViewports`  Const. Length: viewportCount.
  -> m ()
vkCmdSetViewport vk commandBuffer firstViewport viewportCount pViewports =
  liftIO (ffi_vkCmdSetViewport (fp_vkCmdSetViewport vk) commandBuffer firstViewport viewportCount pViewports)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetScissor :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkRect2D -> IO ()) -> (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkRect2D -> IO ())

-- | @vkCmdSetScissor commandBuffer firstScissor scissorCount pScissors@
-- 
-- == Validaty
-- * pname:firstScissor must: be less than sname:VkPhysicalDeviceLimits::pname:maxViewports
-- * The sum of pname:firstScissor and pname:scissorCount must: be between `1` and sname:VkPhysicalDeviceLimits::pname:maxViewports, inclusive
-- * The pname:x and pname:y members of pname:offset must: be greater than or equal to `0`
-- * Evaluation of (pname:offset.x + pname:extent.width) mustnot: cause a signed integer addition overflow
-- * Evaluation of (pname:offset.y + pname:extent.height) mustnot: cause a signed integer addition overflow
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdSetScissor
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Word32
  -- ^ `firstScissor` 
  -> Word32
  -- ^ `scissorCount` 
  -> Ptr VkRect2D
  -- ^ `pScissors`  Const. Length: scissorCount.
  -> m ()
vkCmdSetScissor vk commandBuffer firstScissor scissorCount pScissors =
  liftIO (ffi_vkCmdSetScissor (fp_vkCmdSetScissor vk) commandBuffer firstScissor scissorCount pScissors)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetLineWidth :: FunPtr (VkCommandBuffer -> Float -> IO ()) -> (VkCommandBuffer -> Float -> IO ())

-- | @vkCmdSetLineWidth commandBuffer lineWidth@
-- 
-- == Validaty
-- * If the <<features-features-wideLines,wide lines>> feature is not enabled, pname:lineWidth must: be `1.0`
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdSetLineWidth
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Float
  -- ^ `lineWidth` 
  -> m ()
vkCmdSetLineWidth vk commandBuffer lineWidth =
  liftIO (ffi_vkCmdSetLineWidth (fp_vkCmdSetLineWidth vk) commandBuffer lineWidth)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetDepthBias :: FunPtr (VkCommandBuffer -> Float -> Float -> Float -> IO ()) -> (VkCommandBuffer -> Float -> Float -> Float -> IO ())

-- | @vkCmdSetDepthBias commandBuffer depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor@
-- 
-- == Validaty
-- * If the <<features-features-depthBiasClamp,depth bias clamping>> feature is not enabled, pname:depthBiasClamp must: be code:0.0
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdSetDepthBias
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Float
  -- ^ `depthBiasConstantFactor` 
  -> Float
  -- ^ `depthBiasClamp` 
  -> Float
  -- ^ `depthBiasSlopeFactor` 
  -> m ()
vkCmdSetDepthBias vk commandBuffer depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor =
  liftIO (ffi_vkCmdSetDepthBias (fp_vkCmdSetDepthBias vk) commandBuffer depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetBlendConstants :: FunPtr (VkCommandBuffer -> Float -> Float -> Float -> Float -> IO ()) -> (VkCommandBuffer -> Float -> Float -> Float -> Float -> IO ())

-- | @vkCmdSetBlendConstants commandBuffer blendConstant1 blendConstant2 blendConstant3 blendConstant4@
-- 
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdSetBlendConstants
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Float
  -- ^ `blendConstant1` 
  -> Float
  -- ^ `blendConstant2` 
  -> Float
  -- ^ `blendConstant3` 
  -> Float
  -- ^ `blendConstant4` 
  -> m ()
vkCmdSetBlendConstants vk commandBuffer blendConstant1 blendConstant2 blendConstant3 blendConstant4 =
  liftIO (ffi_vkCmdSetBlendConstants (fp_vkCmdSetBlendConstants vk) commandBuffer blendConstant1 blendConstant2 blendConstant3 blendConstant4)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetDepthBounds :: FunPtr (VkCommandBuffer -> Float -> Float -> IO ()) -> (VkCommandBuffer -> Float -> Float -> IO ())

-- | @vkCmdSetDepthBounds commandBuffer minDepthBounds maxDepthBounds@
-- 
-- == Validaty
-- * pname:minDepthBounds must: be between `0.0` and `1.0`, inclusive
-- * pname:maxDepthBounds must: be between `0.0` and `1.0`, inclusive
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdSetDepthBounds
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Float
  -- ^ `minDepthBounds` 
  -> Float
  -- ^ `maxDepthBounds` 
  -> m ()
vkCmdSetDepthBounds vk commandBuffer minDepthBounds maxDepthBounds =
  liftIO (ffi_vkCmdSetDepthBounds (fp_vkCmdSetDepthBounds vk) commandBuffer minDepthBounds maxDepthBounds)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetStencilCompareMask :: FunPtr (VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()) -> (VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ())

-- | @vkCmdSetStencilCompareMask commandBuffer faceMask compareMask@
-- 
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdSetStencilCompareMask
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkStencilFaceFlags
  -- ^ `faceMask` 
  -> Word32
  -- ^ `compareMask` 
  -> m ()
vkCmdSetStencilCompareMask vk commandBuffer faceMask compareMask =
  liftIO (ffi_vkCmdSetStencilCompareMask (fp_vkCmdSetStencilCompareMask vk) commandBuffer faceMask compareMask)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetStencilWriteMask :: FunPtr (VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()) -> (VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ())

-- | @vkCmdSetStencilWriteMask commandBuffer faceMask writeMask@
-- 
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdSetStencilWriteMask
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkStencilFaceFlags
  -- ^ `faceMask` 
  -> Word32
  -- ^ `writeMask` 
  -> m ()
vkCmdSetStencilWriteMask vk commandBuffer faceMask writeMask =
  liftIO (ffi_vkCmdSetStencilWriteMask (fp_vkCmdSetStencilWriteMask vk) commandBuffer faceMask writeMask)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetStencilReference :: FunPtr (VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()) -> (VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ())

-- | @vkCmdSetStencilReference commandBuffer faceMask reference@
-- 
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdSetStencilReference
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkStencilFaceFlags
  -- ^ `faceMask` 
  -> Word32
  -- ^ `reference` 
  -> m ()
vkCmdSetStencilReference vk commandBuffer faceMask reference =
  liftIO (ffi_vkCmdSetStencilReference (fp_vkCmdSetStencilReference vk) commandBuffer faceMask reference)


foreign import ccall unsafe "dynamic" ffi_vkCmdBindDescriptorSets :: FunPtr (VkCommandBuffer -> VkPipelineBindPoint -> VkPipelineLayout -> Word32 -> Word32 -> Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ()) -> (VkCommandBuffer -> VkPipelineBindPoint -> VkPipelineLayout -> Word32 -> Word32 -> Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ())

-- | @vkCmdBindDescriptorSets commandBuffer pipelineBindPoint layout firstSet descriptorSetCount pDescriptorSets dynamicOffsetCount pDynamicOffsets@
-- 
-- == Validaty
-- * Any given element of pname:pDescriptorSets must: have been created with a sname:VkDescriptorSetLayout that matches (is the same as, or defined identically to) the sname:VkDescriptorSetLayout at set _n_ in pname:layout, where _n_ is the sum of pname:firstSet and the index into pname:pDescriptorSets
-- * pname:dynamicOffsetCount must: be equal to the total number of dynamic descriptors in pname:pDescriptorSets
-- * pname:pipelineBindPoint must: be supported by the pname:commandBuffer's parent sname:VkCommandPool's queue family
-- * Any given element of pname:pDynamicOffsets must: satisfy the required alignment for the corresponding descriptor binding's descriptor type
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdBindDescriptorSets
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkPipelineBindPoint
  -- ^ `pipelineBindPoint` 
  -> VkPipelineLayout
  -- ^ `layout` 
  -> Word32
  -- ^ `firstSet` 
  -> Word32
  -- ^ `descriptorSetCount` 
  -> Ptr VkDescriptorSet
  -- ^ `pDescriptorSets`  Const. Length: descriptorSetCount.
  -> Word32
  -- ^ `dynamicOffsetCount`  Can be `nullPtr`.
  -> Ptr Word32
  -- ^ `pDynamicOffsets`  Const. Length: dynamicOffsetCount.
  -> m ()
vkCmdBindDescriptorSets vk commandBuffer pipelineBindPoint layout firstSet descriptorSetCount pDescriptorSets dynamicOffsetCount pDynamicOffsets =
  liftIO (ffi_vkCmdBindDescriptorSets (fp_vkCmdBindDescriptorSets vk) commandBuffer pipelineBindPoint layout firstSet descriptorSetCount pDescriptorSets dynamicOffsetCount pDynamicOffsets)


foreign import ccall unsafe "dynamic" ffi_vkCmdBindIndexBuffer :: FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkIndexType -> IO ()) -> (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkIndexType -> IO ())

-- | @vkCmdBindIndexBuffer commandBuffer buffer offset indexType@
-- 
-- == Validaty
-- * pname:offset must: be less than the size of pname:buffer
-- * The sum of pname:offset, and the address of the range of sname:VkDeviceMemory object that's backing pname:buffer, must: be a multiple of the type indicated by pname:indexType
-- * pname:buffer must: have been created with the ename:VK_BUFFER_USAGE_INDEX_BUFFER_BIT flag
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdBindIndexBuffer
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkBuffer
  -- ^ `buffer` 
  -> VkDeviceSize
  -- ^ `offset` 
  -> VkIndexType
  -- ^ `indexType` 
  -> m ()
vkCmdBindIndexBuffer vk commandBuffer buffer offset indexType =
  liftIO (ffi_vkCmdBindIndexBuffer (fp_vkCmdBindIndexBuffer vk) commandBuffer buffer offset indexType)


foreign import ccall unsafe "dynamic" ffi_vkCmdBindVertexBuffers :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkBuffer -> Ptr VkDeviceSize -> IO ()) -> (VkCommandBuffer -> Word32 -> Word32 -> Ptr VkBuffer -> Ptr VkDeviceSize -> IO ())

-- | @vkCmdBindVertexBuffers commandBuffer firstBinding bindingCount pBuffers pOffsets@
-- 
-- == Validaty
-- * pname:firstBinding must: be less than sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindings
-- * The sum of pname:firstBinding and pname:bindingCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxVertexInputBindings
-- * All elements of pname:pOffsets must: be less than the size of the corresponding element in pname:pBuffers
-- * All elements of pname:pBuffers must: have been created with the ename:VK_BUFFER_USAGE_VERTEX_BUFFER_BIT flag
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdBindVertexBuffers
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Word32
  -- ^ `firstBinding` 
  -> Word32
  -- ^ `bindingCount` 
  -> Ptr VkBuffer
  -- ^ `pBuffers`  Const. Length: bindingCount.
  -> Ptr VkDeviceSize
  -- ^ `pOffsets`  Const. Length: bindingCount.
  -> m ()
vkCmdBindVertexBuffers vk commandBuffer firstBinding bindingCount pBuffers pOffsets =
  liftIO (ffi_vkCmdBindVertexBuffers (fp_vkCmdBindVertexBuffers vk) commandBuffer firstBinding bindingCount pBuffers pOffsets)


foreign import ccall unsafe "dynamic" ffi_vkCmdDraw :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ())

-- | @vkCmdDraw commandBuffer vertexCount instanceCount firstVertex firstInstance@
-- 
-- == Validaty
-- * For each set _n_ that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS, a descriptor set must: have been bound to _n_ at ename:VK_PIPELINE_BIND_POINT_GRAPHICS, with a sname:VkPipelineLayout that is compatible for set _n_, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * For each push constant that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS, a push constant value must: have been set for ename:VK_PIPELINE_BIND_POINT_GRAPHICS, with a sname:VkPipelineLayout that is compatible for push constants, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * Descriptors in each bound descriptor set, specified via fname:vkCmdBindDescriptorSets, must: be valid if they are statically used by the currently bound sname:VkPipeline object, specified via fname:vkCmdBindPipeline
-- * All vertex input bindings accessed via vertex input variables declared in the vertex shader entry point's interface must: have valid buffers bound
-- * For a given vertex buffer binding, any attribute data fetched must: be entirely contained within the corresponding vertex buffer binding, as described in <<fxvertex-input>>
-- * A valid graphics pipeline must: be bound to the current command buffer with ename:VK_PIPELINE_BIND_POINT_GRAPHICS
-- * If the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS requires any dynamic state, that state must: have been set on the current command buffer
-- * Every input attachment used by the current subpass must: be bound to the pipeline via a descriptor set
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used to sample from any sname:VkImage with a sname:VkImageView of the type ename:VK_IMAGE_VIEW_TYPE_3D, ename:VK_IMAGE_VIEW_TYPE_CUBE, ename:VK_IMAGE_VIEW_TYPE_1D_ARRAY, ename:VK_IMAGE_VIEW_TYPE_2D_ARRAY or ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions with code:ImplicitLod, code:Dref or code:Proj in their name, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions that includes a lod bias or any offset values, in any shader stage
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS accesses a uniform buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS accesses a storage buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * Any sname:VkImageView being sampled with ename:VK_FILTER_LINEAR as a result of this command must: be of a format which supports linear filtering, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures (for a linear image) or sname:VkFormatProperties::pname:optimalTilingFeatures(for an optimally tiled image) returned by fname:vkGetPhysicalDeviceFormatProperties
-- 
-- s: e: q: rp:inside cbl:primary,secondary iesp:[]
vkCmdDraw
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Word32
  -- ^ `vertexCount` 
  -> Word32
  -- ^ `instanceCount` 
  -> Word32
  -- ^ `firstVertex` 
  -> Word32
  -- ^ `firstInstance` 
  -> m ()
vkCmdDraw vk commandBuffer vertexCount instanceCount firstVertex firstInstance =
  liftIO (ffi_vkCmdDraw (fp_vkCmdDraw vk) commandBuffer vertexCount instanceCount firstVertex firstInstance)


foreign import ccall unsafe "dynamic" ffi_vkCmdDrawIndexed :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()) -> (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ())

-- | @vkCmdDrawIndexed commandBuffer indexCount instanceCount firstIndex vertexOffset firstInstance@
-- 
-- == Validaty
-- * For each set _n_ that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS, a descriptor set must: have been bound to _n_ at ename:VK_PIPELINE_BIND_POINT_GRAPHICS, with a sname:VkPipelineLayout that is compatible for set _n_, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * For each push constant that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS, a push constant value must: have been set for ename:VK_PIPELINE_BIND_POINT_GRAPHICS, with a sname:VkPipelineLayout that is compatible for push constants, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * Descriptors in each bound descriptor set, specified via fname:vkCmdBindDescriptorSets, must: be valid if they are statically used by the currently bound sname:VkPipeline object, specified via fname:vkCmdBindPipeline
-- * All vertex input bindings accessed via vertex input variables declared in the vertex shader entry point's interface must: have valid buffers bound
-- * For a given vertex buffer binding, any attribute data fetched must: be entirely contained within the corresponding vertex buffer binding, as described in <<fxvertex-input>>
-- * A valid graphics pipeline must: be bound to the current command buffer with ename:VK_PIPELINE_BIND_POINT_GRAPHICS
-- * If the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS requires any dynamic state, that state must: have been set on the current command buffer
-- * (pname:indexSize * (pname:firstIndex + pname:indexCount) + pname:offset) must: be less than or equal to the size of the currently bound index buffer, with indexSize being based on the type specified by pname:indexType, where the index buffer, pname:indexType, and pname:offset are specified via fname:vkCmdBindIndexBuffer
-- * Every input attachment used by the current subpass must: be bound to the pipeline via a descriptor set
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used to sample from any sname:VkImage with a sname:VkImageView of the type ename:VK_IMAGE_VIEW_TYPE_3D, ename:VK_IMAGE_VIEW_TYPE_CUBE, ename:VK_IMAGE_VIEW_TYPE_1D_ARRAY, ename:VK_IMAGE_VIEW_TYPE_2D_ARRAY or ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions with code:ImplicitLod, code:Dref or code:Proj in their name, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions that includes a lod bias or any offset values, in any shader stage
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS accesses a uniform buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS accesses a storage buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * Any sname:VkImageView being sampled with ename:VK_FILTER_LINEAR as a result of this command must: be of a format which supports linear filtering, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures (for a linear image) or sname:VkFormatProperties::pname:optimalTilingFeatures(for an optimally tiled image) returned by fname:vkGetPhysicalDeviceFormatProperties
-- 
-- s: e: q: rp:inside cbl:primary,secondary iesp:[]
vkCmdDrawIndexed
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Word32
  -- ^ `indexCount` 
  -> Word32
  -- ^ `instanceCount` 
  -> Word32
  -- ^ `firstIndex` 
  -> Int32
  -- ^ `vertexOffset` 
  -> Word32
  -- ^ `firstInstance` 
  -> m ()
vkCmdDrawIndexed vk commandBuffer indexCount instanceCount firstIndex vertexOffset firstInstance =
  liftIO (ffi_vkCmdDrawIndexed (fp_vkCmdDrawIndexed vk) commandBuffer indexCount instanceCount firstIndex vertexOffset firstInstance)


foreign import ccall unsafe "dynamic" ffi_vkCmdDrawIndirect :: FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()) -> (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ())

-- | @vkCmdDrawIndirect commandBuffer buffer offset drawCount stride@
-- 
-- == Validaty
-- * pname:offset must: be a multiple of `4`
-- * If pname:drawCount is greater than `1`, pname:stride must: be a multiple of `4` and must: be greater than or equal to sizeof(sname:VkDrawIndirectCommand)
-- * If the <<features-features-multiDrawIndirect,multi-draw indirect>> feature is not enabled, pname:drawCount must: be `0` or `1`
-- * If the <<features-features-drawIndirectFirstInstance,drawIndirectFirstInstance>> feature is not enabled, all the pname:firstInstance members of the sname:VkDrawIndirectCommand structures accessed by this command must: be code:0
-- * For each set _n_ that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS, a descriptor set must: have been bound to _n_ at ename:VK_PIPELINE_BIND_POINT_GRAPHICS, with a sname:VkPipelineLayout that is compatible for set _n_, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * For each push constant that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS, a push constant value must: have been set for ename:VK_PIPELINE_BIND_POINT_GRAPHICS, with a sname:VkPipelineLayout that is compatible for push constants, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * Descriptors in each bound descriptor set, specified via fname:vkCmdBindDescriptorSets, must: be valid if they are statically used by the currently bound sname:VkPipeline object, specified via fname:vkCmdBindPipeline
-- * All vertex input bindings accessed via vertex input variables declared in the vertex shader entry point's interface must: have valid buffers bound
-- * A valid graphics pipeline must: be bound to the current command buffer with ename:VK_PIPELINE_BIND_POINT_GRAPHICS
-- * If the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS requires any dynamic state, that state must: have been set on the current command buffer
-- * If pname:drawCount is equal to `1`, (pname:offset + sizeof(sname:VkDrawIndirectCommand)) must: be less than or equal to the size of pname:buffer
-- * If pname:drawCount is greater than `1`, (pname:stride x (pname:drawCount - 1) + pname:offset + sizeof(sname:VkDrawIndirectCommand)) must: be less than or equal to the size of pname:buffer
-- * pname:drawCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxDrawIndirectCount
-- * Every input attachment used by the current subpass must: be bound to the pipeline via a descriptor set
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used to sample from any sname:VkImage with a sname:VkImageView of the type ename:VK_IMAGE_VIEW_TYPE_3D, ename:VK_IMAGE_VIEW_TYPE_CUBE, ename:VK_IMAGE_VIEW_TYPE_1D_ARRAY, ename:VK_IMAGE_VIEW_TYPE_2D_ARRAY or ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions with code:ImplicitLod, code:Dref or code:Proj in their name, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions that includes a lod bias or any offset values, in any shader stage
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS accesses a uniform buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS accesses a storage buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * Any sname:VkImageView being sampled with ename:VK_FILTER_LINEAR as a result of this command must: be of a format which supports linear filtering, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures (for a linear image) or sname:VkFormatProperties::pname:optimalTilingFeatures(for an optimally tiled image) returned by fname:vkGetPhysicalDeviceFormatProperties
-- 
-- s: e: q: rp:inside cbl:primary,secondary iesp:[]
vkCmdDrawIndirect
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkBuffer
  -- ^ `buffer` 
  -> VkDeviceSize
  -- ^ `offset` 
  -> Word32
  -- ^ `drawCount` 
  -> Word32
  -- ^ `stride` 
  -> m ()
vkCmdDrawIndirect vk commandBuffer buffer offset drawCount stride =
  liftIO (ffi_vkCmdDrawIndirect (fp_vkCmdDrawIndirect vk) commandBuffer buffer offset drawCount stride)


foreign import ccall unsafe "dynamic" ffi_vkCmdDrawIndexedIndirect :: FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()) -> (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ())

-- | @vkCmdDrawIndexedIndirect commandBuffer buffer offset drawCount stride@
-- 
-- == Validaty
-- * pname:offset must: be a multiple of `4`
-- * If pname:drawCount is greater than `1`, pname:stride must: be a multiple of `4` and must: be greater than or equal to sizeof(sname:VkDrawIndexedIndirectCommand)
-- * If the <<features-features-multiDrawIndirect,multi-draw indirect>> feature is not enabled, pname:drawCount must: be `0` or `1`
-- * If the <<features-features-drawIndirectFirstInstance,drawIndirectFirstInstance>> feature is not enabled, all the pname:firstInstance members of the sname:VkDrawIndexedIndirectCommand structures accessed by this command must: be code:0
-- * For each set _n_ that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS, a descriptor set must: have been bound to _n_ at ename:VK_PIPELINE_BIND_POINT_GRAPHICS, with a sname:VkPipelineLayout that is compatible for set _n_, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * For each push constant that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS, a push constant value must: have been set for ename:VK_PIPELINE_BIND_POINT_GRAPHICS, with a sname:VkPipelineLayout that is compatible for push constants, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * Descriptors in each bound descriptor set, specified via fname:vkCmdBindDescriptorSets, must: be valid if they are statically used by the currently bound sname:VkPipeline object, specified via fname:vkCmdBindPipeline
-- * All vertex input bindings accessed via vertex input variables declared in the vertex shader entry point's interface must: have valid buffers bound
-- * A valid graphics pipeline must: be bound to the current command buffer with ename:VK_PIPELINE_BIND_POINT_GRAPHICS
-- * If the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS requires any dynamic state, that state must: have been set on the current command buffer
-- * If pname:drawCount is equal to `1`, (pname:offset + sizeof(sname:VkDrawIndexedIndirectCommand)) must: be less than or equal to the size of pname:buffer
-- * If pname:drawCount is greater than `1`, (pname:stride x (pname:drawCount - 1) + pname:offset + sizeof(sname:VkDrawIndexedIndirectCommand)) must: be less than or equal to the size of pname:buffer
-- * pname:drawCount must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxDrawIndirectCount
-- * Every input attachment used by the current subpass must: be bound to the pipeline via a descriptor set
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used to sample from any sname:VkImage with a sname:VkImageView of the type ename:VK_IMAGE_VIEW_TYPE_3D, ename:VK_IMAGE_VIEW_TYPE_CUBE, ename:VK_IMAGE_VIEW_TYPE_1D_ARRAY, ename:VK_IMAGE_VIEW_TYPE_2D_ARRAY or ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions with code:ImplicitLod, code:Dref or code:Proj in their name, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions that includes a lod bias or any offset values, in any shader stage
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS accesses a uniform buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_GRAPHICS accesses a storage buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * Any sname:VkImageView being sampled with ename:VK_FILTER_LINEAR as a result of this command must: be of a format which supports linear filtering, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures (for a linear image) or sname:VkFormatProperties::pname:optimalTilingFeatures(for an optimally tiled image) returned by fname:vkGetPhysicalDeviceFormatProperties
-- 
-- s: e: q: rp:inside cbl:primary,secondary iesp:[]
vkCmdDrawIndexedIndirect
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkBuffer
  -- ^ `buffer` 
  -> VkDeviceSize
  -- ^ `offset` 
  -> Word32
  -- ^ `drawCount` 
  -> Word32
  -- ^ `stride` 
  -> m ()
vkCmdDrawIndexedIndirect vk commandBuffer buffer offset drawCount stride =
  liftIO (ffi_vkCmdDrawIndexedIndirect (fp_vkCmdDrawIndexedIndirect vk) commandBuffer buffer offset drawCount stride)


foreign import ccall unsafe "dynamic" ffi_vkCmdDispatch :: FunPtr (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> IO ()) -> (VkCommandBuffer -> Word32 -> Word32 -> Word32 -> IO ())

-- | @vkCmdDispatch commandBuffer x y z@
-- 
-- == Validaty
-- * pname:x must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxComputeWorkGroupCount[0]
-- * pname:y must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxComputeWorkGroupCount[1]
-- * pname:z must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxComputeWorkGroupCount[2]
-- * For each set _n_ that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE, a descriptor set must: have been bound to _n_ at ename:VK_PIPELINE_BIND_POINT_COMPUTE, with a sname:VkPipelineLayout that is compatible for set _n_, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * Descriptors in each bound descriptor set, specified via fname:vkCmdBindDescriptorSets, must: be valid if they are statically used by the currently bound sname:VkPipeline object, specified via fname:vkCmdBindPipeline
-- * A valid compute pipeline must: be bound to the current command buffer with ename:VK_PIPELINE_BIND_POINT_COMPUTE
-- * For each push constant that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE, a push constant value must: have been set for ename:VK_PIPELINE_BIND_POINT_COMPUTE, with a sname:VkPipelineLayout that is compatible for push constants with the one used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE uses unnormalized coordinates, it mustnot: be used to sample from any sname:VkImage with a sname:VkImageView of the type ename:VK_IMAGE_VIEW_TYPE_3D, ename:VK_IMAGE_VIEW_TYPE_CUBE, ename:VK_IMAGE_VIEW_TYPE_1D_ARRAY, ename:VK_IMAGE_VIEW_TYPE_2D_ARRAY or ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions with code:ImplicitLod, code:Dref or code:Proj in their name, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions that includes a lod bias or any offset values, in any shader stage
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE accesses a uniform buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE accesses a storage buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * Any sname:VkImageView being sampled with ename:VK_FILTER_LINEAR as a result of this command must: be of a format which supports linear filtering, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures (for a linear image) or sname:VkFormatProperties::pname:optimalTilingFeatures(for an optimally tiled image) returned by fname:vkGetPhysicalDeviceFormatProperties
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdDispatch
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Word32
  -- ^ `x` 
  -> Word32
  -- ^ `y` 
  -> Word32
  -- ^ `z` 
  -> m ()
vkCmdDispatch vk commandBuffer x y z =
  liftIO (ffi_vkCmdDispatch (fp_vkCmdDispatch vk) commandBuffer x y z)


foreign import ccall unsafe "dynamic" ffi_vkCmdDispatchIndirect :: FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> IO ()) -> (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> IO ())

-- | @vkCmdDispatchIndirect commandBuffer buffer offset@
-- 
-- == Validaty
-- * For each set _n_ that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE, a descriptor set must: have been bound to _n_ at ename:VK_PIPELINE_BIND_POINT_COMPUTE, with a sname:VkPipelineLayout that is compatible for set _n_, with the sname:VkPipelineLayout used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * Descriptors in each bound descriptor set, specified via fname:vkCmdBindDescriptorSets, must: be valid if they are statically used by the currently bound sname:VkPipeline object, specified via fname:vkCmdBindPipeline
-- * A valid compute pipeline must: be bound to the current command buffer with ename:VK_PIPELINE_BIND_POINT_COMPUTE
-- * pname:buffer must: have been created with the ename:VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT bit set
-- * pname:offset must: be a multiple of `4`
-- * The sum of pname:offset and the size of sname:VkDispatchIndirectCommand must: be less than or equal to the size of pname:buffer
-- * For each push constant that is statically used by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE, a push constant value must: have been set for ename:VK_PIPELINE_BIND_POINT_COMPUTE, with a sname:VkPipelineLayout that is compatible for push constants with the one used to create the current sname:VkPipeline, as described in <<descriptorsets-compatibility>>
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE uses unnormalized coordinates, it mustnot: be used to sample from any sname:VkImage with a sname:VkImageView of the type ename:VK_IMAGE_VIEW_TYPE_3D, ename:VK_IMAGE_VIEW_TYPE_CUBE, ename:VK_IMAGE_VIEW_TYPE_1D_ARRAY, ename:VK_IMAGE_VIEW_TYPE_2D_ARRAY or ename:VK_IMAGE_VIEW_TYPE_CUBE_ARRAY, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions with code:ImplicitLod, code:Dref or code:Proj in their name, in any shader stage
-- * If any sname:VkSampler object that is accessed from a shader by the sname:VkPipeline currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE uses unnormalized coordinates, it mustnot: be used with any of the SPIR-V `OpImageSample*` or `OpImageSparseSample*` instructions that includes a lod bias or any offset values, in any shader stage
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE accesses a uniform buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * If the <<features-features-robustBufferAccess,robust buffer access>> feature is not enabled, and any shader stage in the sname:VkPipeline object currently bound to ename:VK_PIPELINE_BIND_POINT_COMPUTE accesses a storage buffer, it mustnot: access values outside of the range of that buffer specified in the currently bound descriptor set
-- * Any sname:VkImageView being sampled with ename:VK_FILTER_LINEAR as a result of this command must: be of a format which supports linear filtering, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures (for a linear image) or sname:VkFormatProperties::pname:optimalTilingFeatures(for an optimally tiled image) returned by fname:vkGetPhysicalDeviceFormatProperties
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdDispatchIndirect
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkBuffer
  -- ^ `buffer` 
  -> VkDeviceSize
  -- ^ `offset` 
  -> m ()
vkCmdDispatchIndirect vk commandBuffer buffer offset =
  liftIO (ffi_vkCmdDispatchIndirect (fp_vkCmdDispatchIndirect vk) commandBuffer buffer offset)


foreign import ccall unsafe "dynamic" ffi_vkCmdCopyBuffer :: FunPtr (VkCommandBuffer -> VkBuffer -> VkBuffer -> Word32 -> Ptr VkBufferCopy -> IO ()) -> (VkCommandBuffer -> VkBuffer -> VkBuffer -> Word32 -> Ptr VkBufferCopy -> IO ())

-- | @vkCmdCopyBuffer commandBuffer srcBuffer dstBuffer regionCount pRegions@
-- 
-- == Validaty
-- * The pname:size member of a given element of pname:pRegions must: be greater than `0`
-- * The pname:srcOffset member of a given element of pname:pRegions must: be less than the size of pname:srcBuffer
-- * The pname:dstOffset member of a given element of pname:pRegions must: be less than the size of pname:dstBuffer
-- * The pname:size member of a given element of pname:pRegions must: be less than or equal to the size of pname:srcBuffer minus pname:srcOffset
-- * The pname:size member of a given element of pname:pRegions must: be less than or equal to the size of pname:dstBuffer minus pname:dstOffset
-- * The union of the source regions, and the union of the destination regions, specified by the elements of pname:pRegions, mustnot: overlap in memory
-- * pname:srcBuffer must: have been created with ename:VK_BUFFER_USAGE_TRANSFER_SRC_BIT usage flag
-- * pname:dstBuffer must: have been created with ename:VK_BUFFER_USAGE_TRANSFER_DST_BIT usage flag
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdCopyBuffer
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkBuffer
  -- ^ `srcBuffer` 
  -> VkBuffer
  -- ^ `dstBuffer` 
  -> Word32
  -- ^ `regionCount` 
  -> Ptr VkBufferCopy
  -- ^ `pRegions`  Const. Length: regionCount.
  -> m ()
vkCmdCopyBuffer vk commandBuffer srcBuffer dstBuffer regionCount pRegions =
  liftIO (ffi_vkCmdCopyBuffer (fp_vkCmdCopyBuffer vk) commandBuffer srcBuffer dstBuffer regionCount pRegions)


foreign import ccall unsafe "dynamic" ffi_vkCmdCopyImage :: FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageCopy -> IO ()) -> (VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageCopy -> IO ())

-- | @vkCmdCopyImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regionCount pRegions@
-- 
-- == Validaty
-- * The source region specified by a given element of pname:pRegions must: be a region that is contained within pname:srcImage
-- * The destination region specified by a given element of pname:pRegions must: be a region that is contained within pname:dstImage
-- * The union of all source regions, and the union of all destination regions, specified by the elements of pname:pRegions, mustnot: overlap in memory
-- * pname:srcImage must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_SRC_BIT usage flag
-- * pname:srcImageLayout must: specify the layout of the image subresources of pname:srcImage specified in pname:pRegions at the time this command is executed on a sname:VkDevice
-- * pname:srcImageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- * pname:dstImage must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_DST_BIT usage flag
-- * pname:dstImageLayout must: specify the layout of the image subresources of pname:dstImage specified in pname:pRegions at the time this command is executed on a sname:VkDevice
-- * pname:dstImageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- * The elink:VkFormat of each of pname:srcImage and pname:dstImage must: be compatible, as defined <<copies-images-format-compatibility, below>>
-- * The sample count of pname:srcImage and pname:dstImage must: match
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdCopyImage
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkImage
  -- ^ `srcImage` 
  -> VkImageLayout
  -- ^ `srcImageLayout` 
  -> VkImage
  -- ^ `dstImage` 
  -> VkImageLayout
  -- ^ `dstImageLayout` 
  -> Word32
  -- ^ `regionCount` 
  -> Ptr VkImageCopy
  -- ^ `pRegions`  Const. Length: regionCount.
  -> m ()
vkCmdCopyImage vk commandBuffer srcImage srcImageLayout dstImage dstImageLayout regionCount pRegions =
  liftIO (ffi_vkCmdCopyImage (fp_vkCmdCopyImage vk) commandBuffer srcImage srcImageLayout dstImage dstImageLayout regionCount pRegions)


foreign import ccall unsafe "dynamic" ffi_vkCmdBlitImage :: FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageBlit -> VkFilter -> IO ()) -> (VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageBlit -> VkFilter -> IO ())

-- | @vkCmdBlitImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regionCount pRegions filter@
-- 
-- == Validaty
-- * The source region specified by a given element of pname:pRegions must: be a region that is contained within pname:srcImage
-- * The destination region specified by a given element of pname:pRegions must: be a region that is contained within pname:dstImage
-- * The union of all source regions, and the union of all destination regions, specified by the elements of pname:pRegions, mustnot: overlap in memory
-- * pname:srcImage must: use a format that supports ename:VK_FORMAT_FEATURE_BLIT_SRC_BIT, which is indicated by sname:VkFormatProperties::pname:linearTilingFeatures (for linear tiled images) or sname:VkFormatProperties::pname:optimalTilingFeatures (for optimally tiled images) - as returned by fname:vkGetPhysicalDeviceFormatProperties
-- * pname:srcImage must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_SRC_BIT usage flag
-- * pname:srcImageLayout must: specify the layout of the image subresources of pname:srcImage specified in pname:pRegions at the time this command is executed on a sname:VkDevice
-- * pname:srcImageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- * pname:dstImage must: use a format that supports ename:VK_FORMAT_FEATURE_BLIT_DST_BIT, which is indicated by sname:VkFormatProperties::pname:linearTilingFeatures (for linear tiled images) or sname:VkFormatProperties::pname:optimalTilingFeatures (for optimally tiled images) - as returned by fname:vkGetPhysicalDeviceFormatProperties
-- * pname:dstImage must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_DST_BIT usage flag
-- * pname:dstImageLayout must: specify the layout of the image subresources of pname:dstImage specified in pname:pRegions at the time this command is executed on a sname:VkDevice
-- * pname:dstImageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- * The sample count of pname:srcImage and pname:dstImage must: both be equal to ename:VK_SAMPLE_COUNT_1_BIT
-- * If either of pname:srcImage or pname:dstImage was created with a signed integer elink:VkFormat, the other must: also have been created with a signed integer elink:VkFormat
-- * If either of pname:srcImage or pname:dstImage was created with an unsigned integer elink:VkFormat, the other must: also have been created with an unsigned integer elink:VkFormat
-- * If either of pname:srcImage or pname:dstImage was created with a depth/stencil format, the other must: have exactly the same format
-- * If pname:srcImage was created with a depth/stencil format, pname:filter must: be ename:VK_FILTER_NEAREST
-- * If pname:filter is ename:VK_FILTER_LINEAR, pname:srcImage must: be of a format which supports linear filtering, as specified by the ename:VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures (for a linear image) or sname:VkFormatProperties::pname:optimalTilingFeatures(for an optimally tiled image) returned by fname:vkGetPhysicalDeviceFormatProperties
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdBlitImage
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkImage
  -- ^ `srcImage` 
  -> VkImageLayout
  -- ^ `srcImageLayout` 
  -> VkImage
  -- ^ `dstImage` 
  -> VkImageLayout
  -- ^ `dstImageLayout` 
  -> Word32
  -- ^ `regionCount` 
  -> Ptr VkImageBlit
  -- ^ `pRegions`  Const. Length: regionCount.
  -> VkFilter
  -- ^ `filter` 
  -> m ()
vkCmdBlitImage vk commandBuffer srcImage srcImageLayout dstImage dstImageLayout regionCount pRegions filter =
  liftIO (ffi_vkCmdBlitImage (fp_vkCmdBlitImage vk) commandBuffer srcImage srcImageLayout dstImage dstImageLayout regionCount pRegions filter)


foreign import ccall unsafe "dynamic" ffi_vkCmdCopyBufferToImage :: FunPtr (VkCommandBuffer -> VkBuffer -> VkImage -> VkImageLayout -> Word32 -> Ptr VkBufferImageCopy -> IO ()) -> (VkCommandBuffer -> VkBuffer -> VkImage -> VkImageLayout -> Word32 -> Ptr VkBufferImageCopy -> IO ())

-- | @vkCmdCopyBufferToImage commandBuffer srcBuffer dstImage dstImageLayout regionCount pRegions@
-- 
-- == Validaty
-- * The buffer region specified by a given element of pname:pRegions must: be a region that is contained within pname:srcBuffer
-- * The image region specified by a given element of pname:pRegions must: be a region that is contained within pname:dstImage
-- * The union of all source regions, and the union of all destination regions, specified by the elements of pname:pRegions, mustnot: overlap in memory
-- * pname:srcBuffer must: have been created with ename:VK_BUFFER_USAGE_TRANSFER_SRC_BIT usage flag
-- * pname:dstImage must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_DST_BIT usage flag
-- * pname:dstImage must: have a sample count equal to ename:VK_SAMPLE_COUNT_1_BIT
-- * pname:dstImageLayout must: specify the layout of the image subresources of pname:dstImage specified in pname:pRegions at the time this command is executed on a sname:VkDevice
-- * pname:dstImageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdCopyBufferToImage
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkBuffer
  -- ^ `srcBuffer` 
  -> VkImage
  -- ^ `dstImage` 
  -> VkImageLayout
  -- ^ `dstImageLayout` 
  -> Word32
  -- ^ `regionCount` 
  -> Ptr VkBufferImageCopy
  -- ^ `pRegions`  Const. Length: regionCount.
  -> m ()
vkCmdCopyBufferToImage vk commandBuffer srcBuffer dstImage dstImageLayout regionCount pRegions =
  liftIO (ffi_vkCmdCopyBufferToImage (fp_vkCmdCopyBufferToImage vk) commandBuffer srcBuffer dstImage dstImageLayout regionCount pRegions)


foreign import ccall unsafe "dynamic" ffi_vkCmdCopyImageToBuffer :: FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> VkBuffer -> Word32 -> Ptr VkBufferImageCopy -> IO ()) -> (VkCommandBuffer -> VkImage -> VkImageLayout -> VkBuffer -> Word32 -> Ptr VkBufferImageCopy -> IO ())

-- | @vkCmdCopyImageToBuffer commandBuffer srcImage srcImageLayout dstBuffer regionCount pRegions@
-- 
-- == Validaty
-- * The image region specified by a given element of pname:pRegions must: be a region that is contained within pname:srcImage
-- * The buffer region specified by a given element of pname:pRegions must: be a region that is contained within pname:dstBuffer
-- * The union of all source regions, and the union of all destination regions, specified by the elements of pname:pRegions, mustnot: overlap in memory
-- * pname:srcImage must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_SRC_BIT usage flag
-- * pname:srcImage must: have a sample count equal to ename:VK_SAMPLE_COUNT_1_BIT
-- * pname:srcImageLayout must: specify the layout of the image subresources of pname:srcImage specified in pname:pRegions at the time this command is executed on a sname:VkDevice
-- * pname:srcImageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- * pname:dstBuffer must: have been created with ename:VK_BUFFER_USAGE_TRANSFER_DST_BIT usage flag
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdCopyImageToBuffer
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkImage
  -- ^ `srcImage` 
  -> VkImageLayout
  -- ^ `srcImageLayout` 
  -> VkBuffer
  -- ^ `dstBuffer` 
  -> Word32
  -- ^ `regionCount` 
  -> Ptr VkBufferImageCopy
  -- ^ `pRegions`  Const. Length: regionCount.
  -> m ()
vkCmdCopyImageToBuffer vk commandBuffer srcImage srcImageLayout dstBuffer regionCount pRegions =
  liftIO (ffi_vkCmdCopyImageToBuffer (fp_vkCmdCopyImageToBuffer vk) commandBuffer srcImage srcImageLayout dstBuffer regionCount pRegions)


foreign import ccall unsafe "dynamic" ffi_vkCmdUpdateBuffer :: FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> Ptr Word32 -> IO ()) -> (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> Ptr Word32 -> IO ())

-- | @vkCmdUpdateBuffer commandBuffer dstBuffer dstOffset dataSize pData@
-- 
-- == Validaty
-- * pname:dataSize must: be greater than `0`
-- * pname:dstOffset must: be less than the size of pname:dstBuffer
-- * pname:dataSize must: be less than or equal to the size of pname:dstBuffer minus pname:dstOffset
-- * pname:dstBuffer must: have been created with ename:VK_BUFFER_USAGE_TRANSFER_DST_BIT usage flag
-- * pname:dstOffset must: be a multiple of `4`
-- * pname:dataSize must: be less than or equal to `65536`
-- * pname:dataSize must: be a multiple of `4`
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdUpdateBuffer
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkBuffer
  -- ^ `dstBuffer` 
  -> VkDeviceSize
  -- ^ `dstOffset` 
  -> VkDeviceSize
  -- ^ `dataSize` 
  -> Ptr Word32
  -- ^ `pData`  Const. Length: latexmath:[$dataSize \over 4$].
  -> m ()
vkCmdUpdateBuffer vk commandBuffer dstBuffer dstOffset dataSize pData =
  liftIO (ffi_vkCmdUpdateBuffer (fp_vkCmdUpdateBuffer vk) commandBuffer dstBuffer dstOffset dataSize pData)


foreign import ccall unsafe "dynamic" ffi_vkCmdFillBuffer :: FunPtr (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> Word32 -> IO ()) -> (VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> Word32 -> IO ())

-- | @vkCmdFillBuffer commandBuffer dstBuffer dstOffset size word@
-- 
-- == Validaty
-- * pname:dstOffset must: be less than the size of pname:dstBuffer
-- * pname:dstOffset must: be a multiple of `4`
-- * If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be greater than `0`
-- * If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be less than or equal to the size of pname:dstBuffer minus pname:dstOffset
-- * If pname:size is not equal to ename:VK_WHOLE_SIZE, pname:size must: be a multiple of `4`
-- * pname:dstBuffer must: have been created with ename:VK_BUFFER_USAGE_TRANSFER_DST_BIT usage flag
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdFillBuffer
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkBuffer
  -- ^ `dstBuffer` 
  -> VkDeviceSize
  -- ^ `dstOffset` 
  -> VkDeviceSize
  -- ^ `size` 
  -> Word32
  -- ^ `word` 
  -> m ()
vkCmdFillBuffer vk commandBuffer dstBuffer dstOffset size word =
  liftIO (ffi_vkCmdFillBuffer (fp_vkCmdFillBuffer vk) commandBuffer dstBuffer dstOffset size word)


foreign import ccall unsafe "dynamic" ffi_vkCmdClearColorImage :: FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> Ptr VkClearColorValue -> Word32 -> Ptr VkImageSubresourceRange -> IO ()) -> (VkCommandBuffer -> VkImage -> VkImageLayout -> Ptr VkClearColorValue -> Word32 -> Ptr VkImageSubresourceRange -> IO ())

-- | @vkCmdClearColorImage commandBuffer image imageLayout pColor rangeCount pRanges@
-- 
-- == Validaty
-- * pname:image must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_DST_BIT usage flag
-- * pname:imageLayout must: specify the layout of the image subresource ranges of pname:image specified in pname:pRanges at the time this command is executed on a sname:VkDevice
-- * pname:imageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- * The image range of any given element of pname:pRanges must: be an image subresource range that is contained within pname:image
-- * pname:image mustnot: have a compressed or depth/stencil format
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdClearColorImage
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkImage
  -- ^ `image` 
  -> VkImageLayout
  -- ^ `imageLayout` 
  -> Ptr VkClearColorValue
  -- ^ `pColor`  Const.
  -> Word32
  -- ^ `rangeCount` 
  -> Ptr VkImageSubresourceRange
  -- ^ `pRanges`  Const. Length: rangeCount.
  -> m ()
vkCmdClearColorImage vk commandBuffer image imageLayout pColor rangeCount pRanges =
  liftIO (ffi_vkCmdClearColorImage (fp_vkCmdClearColorImage vk) commandBuffer image imageLayout pColor rangeCount pRanges)


foreign import ccall unsafe "dynamic" ffi_vkCmdClearDepthStencilImage :: FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> Ptr VkClearDepthStencilValue -> Word32 -> Ptr VkImageSubresourceRange -> IO ()) -> (VkCommandBuffer -> VkImage -> VkImageLayout -> Ptr VkClearDepthStencilValue -> Word32 -> Ptr VkImageSubresourceRange -> IO ())

-- | @vkCmdClearDepthStencilImage commandBuffer image imageLayout pDepthStencil rangeCount pRanges@
-- 
-- == Validaty
-- * pname:image must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_DST_BIT usage flag
-- * pname:imageLayout must: specify the layout of the image subresource ranges of pname:image specified in pname:pRanges at the time this command is executed on a sname:VkDevice
-- * pname:imageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- * The image range of any given element of pname:pRanges must: be an image subresource range that is contained within pname:image
-- * pname:image must: have a depth/stencil format
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdClearDepthStencilImage
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkImage
  -- ^ `image` 
  -> VkImageLayout
  -- ^ `imageLayout` 
  -> Ptr VkClearDepthStencilValue
  -- ^ `pDepthStencil`  Const.
  -> Word32
  -- ^ `rangeCount` 
  -> Ptr VkImageSubresourceRange
  -- ^ `pRanges`  Const. Length: rangeCount.
  -> m ()
vkCmdClearDepthStencilImage vk commandBuffer image imageLayout pDepthStencil rangeCount pRanges =
  liftIO (ffi_vkCmdClearDepthStencilImage (fp_vkCmdClearDepthStencilImage vk) commandBuffer image imageLayout pDepthStencil rangeCount pRanges)


foreign import ccall unsafe "dynamic" ffi_vkCmdClearAttachments :: FunPtr (VkCommandBuffer -> Word32 -> Ptr VkClearAttachment -> Word32 -> Ptr VkClearRect -> IO ()) -> (VkCommandBuffer -> Word32 -> Ptr VkClearAttachment -> Word32 -> Ptr VkClearRect -> IO ())

-- | @vkCmdClearAttachments commandBuffer attachmentCount pAttachments rectCount pRects@
-- 
-- == Validaty
-- * If the pname:aspectMask member of any given element of pname:pAttachments contains ename:VK_IMAGE_ASPECT_COLOR_BIT, the pname:colorAttachment member of those elements must: refer to a valid color attachment in the current subpass
-- * The rectangular region specified by a given element of pname:pRects must: be contained within the render area of the current render pass instance
-- * The layers specified by a given element of pname:pRects must: be contained within every attachment that pname:pAttachments refers to
-- 
-- s: e: q: rp:inside cbl:primary,secondary iesp:[]
vkCmdClearAttachments
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Word32
  -- ^ `attachmentCount` 
  -> Ptr VkClearAttachment
  -- ^ `pAttachments`  Const. Length: attachmentCount.
  -> Word32
  -- ^ `rectCount` 
  -> Ptr VkClearRect
  -- ^ `pRects`  Const. Length: rectCount.
  -> m ()
vkCmdClearAttachments vk commandBuffer attachmentCount pAttachments rectCount pRects =
  liftIO (ffi_vkCmdClearAttachments (fp_vkCmdClearAttachments vk) commandBuffer attachmentCount pAttachments rectCount pRects)


foreign import ccall unsafe "dynamic" ffi_vkCmdResolveImage :: FunPtr (VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageResolve -> IO ()) -> (VkCommandBuffer -> VkImage -> VkImageLayout -> VkImage -> VkImageLayout -> Word32 -> Ptr VkImageResolve -> IO ())

-- | @vkCmdResolveImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regionCount pRegions@
-- 
-- == Validaty
-- * The source region specified by a given element of pname:pRegions must: be a region that is contained within pname:srcImage
-- * The destination region specified by a given element of pname:pRegions must: be a region that is contained within pname:dstImage
-- * The union of all source regions, and the union of all destination regions, specified by the elements of pname:pRegions, mustnot: overlap in memory
-- * pname:srcImage must: have a sample count equal to any valid sample count value other than ename:VK_SAMPLE_COUNT_1_BIT
-- * pname:dstImage must: have a sample count equal to ename:VK_SAMPLE_COUNT_1_BIT
-- * pname:srcImageLayout must: specify the layout of the image subresources of pname:srcImage specified in pname:pRegions at the time this command is executed on a sname:VkDevice
-- * pname:srcImageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- * pname:dstImageLayout must: specify the layout of the image subresources of pname:dstImage specified in pname:pRegions at the time this command is executed on a sname:VkDevice
-- * pname:dstImageLayout must: be either of ename:VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL or ename:VK_IMAGE_LAYOUT_GENERAL
-- * If pname:dstImage was created with pname:tiling equal to ename:VK_IMAGE_TILING_LINEAR, pname:dstImage must: have been created with a pname:format that supports being a color attachment, as specified by the ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:linearTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- * If pname:dstImage was created with pname:tiling equal to ename:VK_IMAGE_TILING_OPTIMAL, pname:dstImage must: have been created with a pname:format that supports being a color attachment, as specified by the ename:VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT flag in sname:VkFormatProperties::pname:optimalTilingFeatures returned by fname:vkGetPhysicalDeviceFormatProperties
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdResolveImage
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkImage
  -- ^ `srcImage` 
  -> VkImageLayout
  -- ^ `srcImageLayout` 
  -> VkImage
  -- ^ `dstImage` 
  -> VkImageLayout
  -- ^ `dstImageLayout` 
  -> Word32
  -- ^ `regionCount` 
  -> Ptr VkImageResolve
  -- ^ `pRegions`  Const. Length: regionCount.
  -> m ()
vkCmdResolveImage vk commandBuffer srcImage srcImageLayout dstImage dstImageLayout regionCount pRegions =
  liftIO (ffi_vkCmdResolveImage (fp_vkCmdResolveImage vk) commandBuffer srcImage srcImageLayout dstImage dstImageLayout regionCount pRegions)


foreign import ccall unsafe "dynamic" ffi_vkCmdSetEvent :: FunPtr (VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ()) -> (VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ())

-- | @vkCmdSetEvent commandBuffer event stageMask@
-- 
-- == Validaty
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:stageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:stageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdSetEvent
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkEvent
  -- ^ `event` 
  -> VkPipelineStageFlags
  -- ^ `stageMask` 
  -> m ()
vkCmdSetEvent vk commandBuffer event stageMask =
  liftIO (ffi_vkCmdSetEvent (fp_vkCmdSetEvent vk) commandBuffer event stageMask)


foreign import ccall unsafe "dynamic" ffi_vkCmdResetEvent :: FunPtr (VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ()) -> (VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ())

-- | @vkCmdResetEvent commandBuffer event stageMask@
-- 
-- == Validaty
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:stageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:stageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
-- * When this command executes, pname:event mustnot: be waited on by a fname:vkCmdWaitEvents command that is currently executing
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdResetEvent
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkEvent
  -- ^ `event` 
  -> VkPipelineStageFlags
  -- ^ `stageMask` 
  -> m ()
vkCmdResetEvent vk commandBuffer event stageMask =
  liftIO (ffi_vkCmdResetEvent (fp_vkCmdResetEvent vk) commandBuffer event stageMask)


foreign import ccall unsafe "dynamic" ffi_vkCmdWaitEvents :: FunPtr (VkCommandBuffer -> Word32 -> Ptr VkEvent -> VkPipelineStageFlags -> VkPipelineStageFlags -> Word32 -> Ptr VkMemoryBarrier -> Word32 -> Ptr VkBufferMemoryBarrier -> Word32 -> Ptr VkImageMemoryBarrier -> IO ()) -> (VkCommandBuffer -> Word32 -> Ptr VkEvent -> VkPipelineStageFlags -> VkPipelineStageFlags -> Word32 -> Ptr VkMemoryBarrier -> Word32 -> Ptr VkBufferMemoryBarrier -> Word32 -> Ptr VkImageMemoryBarrier -> IO ())

-- | @vkCmdWaitEvents commandBuffer eventCount pEvents srcStageMask dstStageMask memoryBarrierCount pMemoryBarriers bufferMemoryBarrierCount pBufferMemoryBarriers imageMemoryBarrierCount pImageMemoryBarriers@
-- 
-- == Validaty
-- * pname:srcStageMask must: be the bitwise OR of the pname:stageMask parameter used in previous calls to fname:vkCmdSetEvent with any of the members of pname:pEvents and ename:VK_PIPELINE_STAGE_HOST_BIT if any of the members of pname:pEvents was set using fname:vkSetEvent
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:srcStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:dstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:srcStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:dstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
-- * If pname:pEvents includes one or more events that will be signaled by fname:vkSetEvent after pname:commandBuffer has been submitted to a queue, then fname:vkCmdWaitEvents mustnot: be called inside a render pass instance
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdWaitEvents
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Word32
  -- ^ `eventCount` 
  -> Ptr VkEvent
  -- ^ `pEvents`  Const. Length: eventCount.
  -> VkPipelineStageFlags
  -- ^ `srcStageMask` 
  -> VkPipelineStageFlags
  -- ^ `dstStageMask` 
  -> Word32
  -- ^ `memoryBarrierCount`  Can be `nullPtr`.
  -> Ptr VkMemoryBarrier
  -- ^ `pMemoryBarriers`  Const. Length: memoryBarrierCount.
  -> Word32
  -- ^ `bufferMemoryBarrierCount`  Can be `nullPtr`.
  -> Ptr VkBufferMemoryBarrier
  -- ^ `pBufferMemoryBarriers`  Const. Length: bufferMemoryBarrierCount.
  -> Word32
  -- ^ `imageMemoryBarrierCount`  Can be `nullPtr`.
  -> Ptr VkImageMemoryBarrier
  -- ^ `pImageMemoryBarriers`  Const. Length: imageMemoryBarrierCount.
  -> m ()
vkCmdWaitEvents vk commandBuffer eventCount pEvents srcStageMask dstStageMask memoryBarrierCount pMemoryBarriers bufferMemoryBarrierCount pBufferMemoryBarriers imageMemoryBarrierCount pImageMemoryBarriers =
  liftIO (ffi_vkCmdWaitEvents (fp_vkCmdWaitEvents vk) commandBuffer eventCount pEvents srcStageMask dstStageMask memoryBarrierCount pMemoryBarriers bufferMemoryBarrierCount pBufferMemoryBarriers imageMemoryBarrierCount pImageMemoryBarriers)


foreign import ccall unsafe "dynamic" ffi_vkCmdPipelineBarrier :: FunPtr (VkCommandBuffer -> VkPipelineStageFlags -> VkPipelineStageFlags -> VkDependencyFlags -> Word32 -> Ptr VkMemoryBarrier -> Word32 -> Ptr VkBufferMemoryBarrier -> Word32 -> Ptr VkImageMemoryBarrier -> IO ()) -> (VkCommandBuffer -> VkPipelineStageFlags -> VkPipelineStageFlags -> VkDependencyFlags -> Word32 -> Ptr VkMemoryBarrier -> Word32 -> Ptr VkBufferMemoryBarrier -> Word32 -> Ptr VkImageMemoryBarrier -> IO ())

-- | @vkCmdPipelineBarrier commandBuffer srcStageMask dstStageMask dependencyFlags memoryBarrierCount pMemoryBarriers bufferMemoryBarrierCount pBufferMemoryBarriers imageMemoryBarrierCount pImageMemoryBarriers@
-- 
-- == Validaty
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:srcStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
-- * If the <<features-features-geometryShader,geometry shaders>> feature is not enabled, pname:dstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:srcStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
-- * If the <<features-features-tessellationShader,tessellation shaders>> feature is not enabled, pname:dstStageMask mustnot: contain ename:VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT or ename:VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
-- * If fname:vkCmdPipelineBarrier is called within a render pass instance, the render pass must: declare at least one self-dependency from the current subpass to itself - see <<synchronization-pipeline-barriers-subpass-self-dependencies,Subpass Self-dependency>>
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdPipelineBarrier
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkPipelineStageFlags
  -- ^ `srcStageMask` 
  -> VkPipelineStageFlags
  -- ^ `dstStageMask` 
  -> VkDependencyFlags
  -- ^ `dependencyFlags`  Can be `nullPtr`.
  -> Word32
  -- ^ `memoryBarrierCount`  Can be `nullPtr`.
  -> Ptr VkMemoryBarrier
  -- ^ `pMemoryBarriers`  Const. Length: memoryBarrierCount.
  -> Word32
  -- ^ `bufferMemoryBarrierCount`  Can be `nullPtr`.
  -> Ptr VkBufferMemoryBarrier
  -- ^ `pBufferMemoryBarriers`  Const. Length: bufferMemoryBarrierCount.
  -> Word32
  -- ^ `imageMemoryBarrierCount`  Can be `nullPtr`.
  -> Ptr VkImageMemoryBarrier
  -- ^ `pImageMemoryBarriers`  Const. Length: imageMemoryBarrierCount.
  -> m ()
vkCmdPipelineBarrier vk commandBuffer srcStageMask dstStageMask dependencyFlags memoryBarrierCount pMemoryBarriers bufferMemoryBarrierCount pBufferMemoryBarriers imageMemoryBarrierCount pImageMemoryBarriers =
  liftIO (ffi_vkCmdPipelineBarrier (fp_vkCmdPipelineBarrier vk) commandBuffer srcStageMask dstStageMask dependencyFlags memoryBarrierCount pMemoryBarriers bufferMemoryBarrierCount pBufferMemoryBarriers imageMemoryBarrierCount pImageMemoryBarriers)


foreign import ccall unsafe "dynamic" ffi_vkCmdBeginQuery :: FunPtr (VkCommandBuffer -> VkQueryPool -> Word32 -> VkQueryControlFlags -> IO ()) -> (VkCommandBuffer -> VkQueryPool -> Word32 -> VkQueryControlFlags -> IO ())

-- | @vkCmdBeginQuery commandBuffer queryPool query flags@
-- 
-- == Validaty
-- * The query identified by pname:queryPool and pname:query must: currently not be <<queries-operation-active,active>>
-- * The query identified by pname:queryPool and pname:query must: be unavailable
-- * If the <<features-features-occlusionQueryPrecise,precise occlusion queries>> feature is not enabled, or the pname:queryType used to create pname:queryPool was not ename:VK_QUERY_TYPE_OCCLUSION, pname:flags mustnot: contain ename:VK_QUERY_CONTROL_PRECISE_BIT
-- * pname:queryPool must: have been created with a pname:queryType that differs from that of any other queries that have been made <<queries-operation-active,active>>, and are currently still active within pname:commandBuffer
-- * pname:query must: be less than the number of queries in pname:queryPool
-- * If the pname:queryType used to create pname:queryPool was ename:VK_QUERY_TYPE_OCCLUSION, the sname:VkCommandPool that pname:commandBuffer was created from must: support graphics operations
-- * If the pname:queryType used to create pname:queryPool was ename:VK_QUERY_TYPE_PIPELINE_STATISTICS and any of the pname:pipelineStatistics indicate graphics operations, the sname:VkCommandPool that pname:commandBuffer was created from must: support graphics operations
-- * If the pname:queryType used to create pname:queryPool was ename:VK_QUERY_TYPE_PIPELINE_STATISTICS and any of the pname:pipelineStatistics indicate compute operations, the sname:VkCommandPool that pname:commandBuffer was created from must: support compute operations
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdBeginQuery
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkQueryPool
  -- ^ `queryPool` 
  -> Word32
  -- ^ `query` 
  -> VkQueryControlFlags
  -- ^ `flags`  Can be `nullPtr`.
  -> m ()
vkCmdBeginQuery vk commandBuffer queryPool query flags =
  liftIO (ffi_vkCmdBeginQuery (fp_vkCmdBeginQuery vk) commandBuffer queryPool query flags)


foreign import ccall unsafe "dynamic" ffi_vkCmdEndQuery :: FunPtr (VkCommandBuffer -> VkQueryPool -> Word32 -> IO ()) -> (VkCommandBuffer -> VkQueryPool -> Word32 -> IO ())

-- | @vkCmdEndQuery commandBuffer queryPool query@
-- 
-- == Validaty
-- * The query identified by pname:queryPool and pname:query must: currently be <<queries-operation-active,active>>
-- * pname:query must: be less than the number of queries in pname:queryPool
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdEndQuery
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkQueryPool
  -- ^ `queryPool` 
  -> Word32
  -- ^ `query` 
  -> m ()
vkCmdEndQuery vk commandBuffer queryPool query =
  liftIO (ffi_vkCmdEndQuery (fp_vkCmdEndQuery vk) commandBuffer queryPool query)


foreign import ccall unsafe "dynamic" ffi_vkCmdResetQueryPool :: FunPtr (VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> IO ()) -> (VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> IO ())

-- | @vkCmdResetQueryPool commandBuffer queryPool firstQuery queryCount@
-- 
-- == Validaty
-- * pname:firstQuery must: be less than the number of queries in pname:queryPool
-- * The sum of pname:firstQuery and pname:queryCount must: be less than or equal to the number of queries in pname:queryPool
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdResetQueryPool
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkQueryPool
  -- ^ `queryPool` 
  -> Word32
  -- ^ `firstQuery` 
  -> Word32
  -- ^ `queryCount` 
  -> m ()
vkCmdResetQueryPool vk commandBuffer queryPool firstQuery queryCount =
  liftIO (ffi_vkCmdResetQueryPool (fp_vkCmdResetQueryPool vk) commandBuffer queryPool firstQuery queryCount)


foreign import ccall unsafe "dynamic" ffi_vkCmdWriteTimestamp :: FunPtr (VkCommandBuffer -> VkPipelineStageFlagBits -> VkQueryPool -> Word32 -> IO ()) -> (VkCommandBuffer -> VkPipelineStageFlagBits -> VkQueryPool -> Word32 -> IO ())

-- | @vkCmdWriteTimestamp commandBuffer pipelineStage queryPool query@
-- 
-- == Validaty
-- * The query identified by pname:queryPool and pname:query must: be _unavailable_
-- * The command pool's queue family must: support a non-zero pname:timestampValidBits
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdWriteTimestamp
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkPipelineStageFlagBits
  -- ^ `pipelineStage` 
  -> VkQueryPool
  -- ^ `queryPool` 
  -> Word32
  -- ^ `query` 
  -> m ()
vkCmdWriteTimestamp vk commandBuffer pipelineStage queryPool query =
  liftIO (ffi_vkCmdWriteTimestamp (fp_vkCmdWriteTimestamp vk) commandBuffer pipelineStage queryPool query)


foreign import ccall unsafe "dynamic" ffi_vkCmdCopyQueryPoolResults :: FunPtr (VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> VkQueryResultFlags -> IO ()) -> (VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> VkBuffer -> VkDeviceSize -> VkDeviceSize -> VkQueryResultFlags -> IO ())

-- | @vkCmdCopyQueryPoolResults commandBuffer queryPool firstQuery queryCount dstBuffer dstOffset stride flags@
-- 
-- == Validaty
-- * pname:dstOffset must: be less than the size of pname:dstBuffer
-- * pname:firstQuery must: be less than the number of queries in pname:queryPool
-- * The sum of pname:firstQuery and pname:queryCount must: be less than or equal to the number of queries in pname:queryPool
-- * If ename:VK_QUERY_RESULT_64_BIT is not set in pname:flags then pname:dstOffset and pname:stride must: be multiples of `4`
-- * If ename:VK_QUERY_RESULT_64_BIT is set in pname:flags then pname:dstOffset and pname:stride must: be multiples of `8`
-- * pname:dstBuffer must: have enough storage, from pname:dstOffset, to contain the result of each query, as described <<queries-operation-memorylayout,here>>
-- * pname:dstBuffer must: have been created with ename:VK_BUFFER_USAGE_TRANSFER_DST_BIT usage flag
-- * If the pname:queryType used to create pname:queryPool was ename:VK_QUERY_TYPE_TIMESTAMP, pname:flags mustnot: contain ename:VK_QUERY_RESULT_PARTIAL_BIT
-- 
-- s: e: q: rp:outside cbl:primary,secondary iesp:[]
vkCmdCopyQueryPoolResults
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkQueryPool
  -- ^ `queryPool` 
  -> Word32
  -- ^ `firstQuery` 
  -> Word32
  -- ^ `queryCount` 
  -> VkBuffer
  -- ^ `dstBuffer` 
  -> VkDeviceSize
  -- ^ `dstOffset` 
  -> VkDeviceSize
  -- ^ `stride` 
  -> VkQueryResultFlags
  -- ^ `flags`  Can be `nullPtr`.
  -> m ()
vkCmdCopyQueryPoolResults vk commandBuffer queryPool firstQuery queryCount dstBuffer dstOffset stride flags =
  liftIO (ffi_vkCmdCopyQueryPoolResults (fp_vkCmdCopyQueryPoolResults vk) commandBuffer queryPool firstQuery queryCount dstBuffer dstOffset stride flags)


foreign import ccall unsafe "dynamic" ffi_vkCmdPushConstants :: FunPtr (VkCommandBuffer -> VkPipelineLayout -> VkShaderStageFlags -> Word32 -> Word32 -> Ptr () -> IO ()) -> (VkCommandBuffer -> VkPipelineLayout -> VkShaderStageFlags -> Word32 -> Word32 -> Ptr () -> IO ())

-- | @vkCmdPushConstants commandBuffer layout stageFlags offset size pValues@
-- 
-- == Validaty
-- * pname:stageFlags must: match exactly the shader stages used in pname:layout for the range specified by pname:offset and pname:size
-- * pname:offset must: be a multiple of `4`
-- * pname:size must: be a multiple of `4`
-- * pname:offset must: be less than sname:VkPhysicalDeviceLimits::pname:maxPushConstantsSize
-- * pname:size must: be less than or equal to sname:VkPhysicalDeviceLimits::pname:maxPushConstantsSize minus pname:offset
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdPushConstants
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkPipelineLayout
  -- ^ `layout` 
  -> VkShaderStageFlags
  -- ^ `stageFlags` 
  -> Word32
  -- ^ `offset` 
  -> Word32
  -- ^ `size` 
  -> Ptr ()
  -- ^ `pValues`  Const. Length: size.
  -> m ()
vkCmdPushConstants vk commandBuffer layout stageFlags offset size pValues =
  liftIO (ffi_vkCmdPushConstants (fp_vkCmdPushConstants vk) commandBuffer layout stageFlags offset size pValues)


foreign import ccall unsafe "dynamic" ffi_vkCmdBeginRenderPass :: FunPtr (VkCommandBuffer -> Ptr VkRenderPassBeginInfo -> VkSubpassContents -> IO ()) -> (VkCommandBuffer -> Ptr VkRenderPassBeginInfo -> VkSubpassContents -> IO ())

-- | @vkCmdBeginRenderPass commandBuffer pRenderPassBegin contents@
-- 
-- == Validaty
-- * If any of the pname:initialLayout or pname:finalLayout member of the sname:VkAttachmentDescription structures or the pname:layout member of the sname:VkAttachmentReference structures specified when creating the render pass specified in the pname:renderPass member of pname:pRenderPassBegin is ename:VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL then the corresponding attachment image of the framebuffer specified in the pname:framebuffer member of pname:pRenderPassBegin must: have been created with ename:VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT set
-- * If any of the pname:initialLayout or pname:finalLayout member of the sname:VkAttachmentDescription structures or the pname:layout member of the sname:VkAttachmentReference structures specified when creating the render pass specified in the pname:renderPass member of pname:pRenderPassBegin is ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL or ename:VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL then the corresponding attachment image of the framebuffer specified in the pname:framebuffer member of pname:pRenderPassBegin must: have been created with ename:VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT set
-- * If any of the pname:initialLayout or pname:finalLayout member of the sname:VkAttachmentDescription structures or the pname:layout member of the sname:VkAttachmentReference structures specified when creating the render pass specified in the pname:renderPass member of pname:pRenderPassBegin is ename:VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL then the corresponding attachment image of the framebuffer specified in the pname:framebuffer member of pname:pRenderPassBegin must: have been created with ename:VK_IMAGE_USAGE_SAMPLED_BIT or ename:VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT set
-- * If any of the pname:initialLayout or pname:finalLayout member of the sname:VkAttachmentDescription structures or the pname:layout member of the sname:VkAttachmentReference structures specified when creating the render pass specified in the pname:renderPass member of pname:pRenderPassBegin is ename:VK_IMAGE_LAYOUT_TRANSFER_SRC_BIT then the corresponding attachment image of the framebuffer specified in the pname:framebuffer member of pname:pRenderPassBegin must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_SRC_BIT set
-- * If any of the pname:initialLayout or pname:finalLayout member of the sname:VkAttachmentDescription structures or the pname:layout member of the sname:VkAttachmentReference structures specified when creating the render pass specified in the pname:renderPass member of pname:pRenderPassBegin is ename:VK_IMAGE_LAYOUT_TRANSFER_DST_BIT then the corresponding attachment image of the framebuffer specified in the pname:framebuffer member of pname:pRenderPassBegin must: have been created with ename:VK_IMAGE_USAGE_TRANSFER_DST_BIT set
-- 
-- s: e: q: rp:outside cbl:primary iesp:[]
vkCmdBeginRenderPass
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Ptr VkRenderPassBeginInfo
  -- ^ `pRenderPassBegin`  Const.
  -> VkSubpassContents
  -- ^ `contents` 
  -> m ()
vkCmdBeginRenderPass vk commandBuffer pRenderPassBegin contents =
  liftIO (ffi_vkCmdBeginRenderPass (fp_vkCmdBeginRenderPass vk) commandBuffer pRenderPassBegin contents)


foreign import ccall unsafe "dynamic" ffi_vkCmdNextSubpass :: FunPtr (VkCommandBuffer -> VkSubpassContents -> IO ()) -> (VkCommandBuffer -> VkSubpassContents -> IO ())

-- | @vkCmdNextSubpass commandBuffer contents@
-- 
-- == Validaty
-- * The current subpass index must: be less than the number of subpasses in the render pass minus one
-- 
-- s: e: q: rp:inside cbl:primary iesp:[]
vkCmdNextSubpass
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> VkSubpassContents
  -- ^ `contents` 
  -> m ()
vkCmdNextSubpass vk commandBuffer contents =
  liftIO (ffi_vkCmdNextSubpass (fp_vkCmdNextSubpass vk) commandBuffer contents)


foreign import ccall unsafe "dynamic" ffi_vkCmdEndRenderPass :: FunPtr (VkCommandBuffer -> IO ()) -> (VkCommandBuffer -> IO ())

-- | @vkCmdEndRenderPass commandBuffer@
-- 
-- == Validaty
-- * The current subpass index must: be equal to the number of subpasses in the render pass minus one
-- 
-- s: e: q: rp:inside cbl:primary iesp:[]
vkCmdEndRenderPass
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> m ()
vkCmdEndRenderPass vk commandBuffer =
  liftIO (ffi_vkCmdEndRenderPass (fp_vkCmdEndRenderPass vk) commandBuffer)


foreign import ccall unsafe "dynamic" ffi_vkCmdExecuteCommands :: FunPtr (VkCommandBuffer -> Word32 -> Ptr VkCommandBuffer -> IO ()) -> (VkCommandBuffer -> Word32 -> Ptr VkCommandBuffer -> IO ())

-- | @vkCmdExecuteCommands commandBuffer commandBufferCount pCommandBuffers@
-- 
-- == Validaty
-- * pname:commandBuffer must: have been created with a pname:level of ename:VK_COMMAND_BUFFER_LEVEL_PRIMARY
-- * Any given element of pname:pCommandBuffers must: have been created with a pname:level of ename:VK_COMMAND_BUFFER_LEVEL_SECONDARY
-- * Any given element of pname:pCommandBuffers mustnot: be already pending execution in pname:commandBuffer, or appear twice in pname:pCommandBuffers, unless it was created with the ename:VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT flag
-- * Any given element of pname:pCommandBuffers mustnot: be already pending execution in any other sname:VkCommandBuffer, unless it was created with the ename:VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT flag
-- * Any given element of pname:pCommandBuffers must: be in the executable state
-- * If fname:vkCmdExecuteCommands is being called within a render pass instance, that render pass instance must: have been begun with the pname:contents parameter of fname:vkCmdBeginRenderPass set to ename:VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
-- * If fname:vkCmdExecuteCommands is being called within a render pass instance, any given element of pname:pCommandBuffers must: have been recorded with the ename:VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
-- * If fname:vkCmdExecuteCommands is being called within a render pass instance, any given element of pname:pCommandBuffers must: have been recorded with the pname:subpass member of the pname:inheritanceInfo structure set to the index of the subpass which the given command buffer will be executed in
-- * If fname:vkCmdExecuteCommands is being called within a render pass instance, any given element of pname:pCommandBuffers must: have been recorded with a render pass that is compatible with the current render pass - see <<renderpass-compatibility>>
-- * If fname:vkCmdExecuteCommands is being called within a render pass instance, and any given element of pname:pCommandBuffers was recorded with the pname:framebuffer member of the sname:VkCommandBufferInheritanceInfo structure not equal to sname:VK_NULL_HANDLE, that sname:VkFramebuffer must: be compatible with the sname:VkFramebuffer used in the current render pass instance
-- * If the <<features-features-inheritedQueries,inherited queries>> feature is not enabled, pname:commandBuffer mustnot: have any queries <<queries-operation-active,active>>
-- * If pname:commandBuffer has a ename:VK_QUERY_TYPE_OCCLUSION query <<queries-operation-active,active>>, then each element of pname:pCommandBuffers must: have been recorded with sname:VkCommandBufferBeginInfo::pname:occlusionQueryEnable set to ename:VK_TRUE
-- * If pname:commandBuffer has a ename:VK_QUERY_TYPE_OCCLUSION query <<queries-operation-active,active>>, then each element of pname:pCommandBuffers must: have been recorded with sname:VkCommandBufferBeginInfo::pname:queryFlags having all bits set that are set for the query
-- * If pname:commandBuffer has a ename:VK_QUERY_TYPE_PIPELINE_STATISTICS query <<queries-operation-active,active>>, then each element of pname:pCommandBuffers must: have been recorded with sname:VkCommandBufferInheritanceInfo::pname:pipelineStatistics having all bits set that are set in the sname:VkQueryPool the query uses
-- * Any given element of pname:pCommandBuffers mustnot: begin any query types that are <<queries-operation-active,active>> in pname:commandBuffer
-- 
-- s: e: q: rp:both cbl:primary iesp:[]
vkCmdExecuteCommands
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer`  ExternSync=true.
  -> Word32
  -- ^ `commandBufferCount` 
  -> Ptr VkCommandBuffer
  -- ^ `pCommandBuffers`  Const. Length: commandBufferCount.
  -> m ()
vkCmdExecuteCommands vk commandBuffer commandBufferCount pCommandBuffers =
  liftIO (ffi_vkCmdExecuteCommands (fp_vkCmdExecuteCommands vk) commandBuffer commandBufferCount pCommandBuffers)


foreign import ccall unsafe "dynamic" ffi_vkCreateAndroidSurfaceKHR :: FunPtr (VkInstance -> Ptr VkAndroidSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance -> Ptr VkAndroidSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)

-- | @vkCreateAndroidSurfaceKHR vulkan pCreateInfo pAllocator pSurface@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_NATIVE_WINDOW_IN_USE_KHR q: rp: cbl: iesp:[]
vkCreateAndroidSurfaceKHR
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> Ptr VkAndroidSurfaceCreateInfoKHR
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSurfaceKHR
  -- ^ `pSurface` 
  -> m VkResult
vkCreateAndroidSurfaceKHR vk vulkan pCreateInfo pAllocator pSurface =
  liftIO (ffi_vkCreateAndroidSurfaceKHR (fp_vkCreateAndroidSurfaceKHR vk) vulkan pCreateInfo pAllocator pSurface)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceDisplayPropertiesKHR :: FunPtr (VkPhysicalDevice -> Ptr Word32 -> Ptr VkDisplayPropertiesKHR -> IO VkResult) -> (VkPhysicalDevice -> Ptr Word32 -> Ptr VkDisplayPropertiesKHR -> IO VkResult)

-- | @vkGetPhysicalDeviceDisplayPropertiesKHR physicalDevice pPropertyCount pProperties@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkGetPhysicalDeviceDisplayPropertiesKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Ptr Word32
  -- ^ `pPropertyCount`  Optional=false,true.
  -> Ptr VkDisplayPropertiesKHR
  -- ^ `pProperties`  Can be `nullPtr`. Length: pPropertyCount.
  -> m VkResult
vkGetPhysicalDeviceDisplayPropertiesKHR vk physicalDevice pPropertyCount pProperties =
  liftIO (ffi_vkGetPhysicalDeviceDisplayPropertiesKHR (fp_vkGetPhysicalDeviceDisplayPropertiesKHR vk) physicalDevice pPropertyCount pProperties)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceDisplayPlanePropertiesKHR :: FunPtr (VkPhysicalDevice -> Ptr Word32 -> Ptr VkDisplayPlanePropertiesKHR -> IO VkResult) -> (VkPhysicalDevice -> Ptr Word32 -> Ptr VkDisplayPlanePropertiesKHR -> IO VkResult)

-- | @vkGetPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice pPropertyCount pProperties@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Ptr Word32
  -- ^ `pPropertyCount`  Optional=false,true.
  -> Ptr VkDisplayPlanePropertiesKHR
  -- ^ `pProperties`  Can be `nullPtr`. Length: pPropertyCount.
  -> m VkResult
vkGetPhysicalDeviceDisplayPlanePropertiesKHR vk physicalDevice pPropertyCount pProperties =
  liftIO (ffi_vkGetPhysicalDeviceDisplayPlanePropertiesKHR (fp_vkGetPhysicalDeviceDisplayPlanePropertiesKHR vk) physicalDevice pPropertyCount pProperties)


foreign import ccall unsafe "dynamic" ffi_vkGetDisplayPlaneSupportedDisplaysKHR :: FunPtr (VkPhysicalDevice -> Word32 -> Ptr Word32 -> Ptr VkDisplayKHR -> IO VkResult) -> (VkPhysicalDevice -> Word32 -> Ptr Word32 -> Ptr VkDisplayKHR -> IO VkResult)

-- | @vkGetDisplayPlaneSupportedDisplaysKHR physicalDevice planeIndex pDisplayCount pDisplays@
-- 
-- == Validaty
-- * pname:planeIndex must: be less than the number of display planes supported by the device as determined by calling fname:vkGetPhysicalDeviceDisplayPlanePropertiesKHR
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkGetDisplayPlaneSupportedDisplaysKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Word32
  -- ^ `planeIndex` 
  -> Ptr Word32
  -- ^ `pDisplayCount`  Optional=false,true.
  -> Ptr VkDisplayKHR
  -- ^ `pDisplays`  Can be `nullPtr`. Length: pDisplayCount.
  -> m VkResult
vkGetDisplayPlaneSupportedDisplaysKHR vk physicalDevice planeIndex pDisplayCount pDisplays =
  liftIO (ffi_vkGetDisplayPlaneSupportedDisplaysKHR (fp_vkGetDisplayPlaneSupportedDisplaysKHR vk) physicalDevice planeIndex pDisplayCount pDisplays)


foreign import ccall unsafe "dynamic" ffi_vkGetDisplayModePropertiesKHR :: FunPtr (VkPhysicalDevice -> VkDisplayKHR -> Ptr Word32 -> Ptr VkDisplayModePropertiesKHR -> IO VkResult) -> (VkPhysicalDevice -> VkDisplayKHR -> Ptr Word32 -> Ptr VkDisplayModePropertiesKHR -> IO VkResult)

-- | @vkGetDisplayModePropertiesKHR physicalDevice display pPropertyCount pProperties@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkGetDisplayModePropertiesKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> VkDisplayKHR
  -- ^ `display` 
  -> Ptr Word32
  -- ^ `pPropertyCount`  Optional=false,true.
  -> Ptr VkDisplayModePropertiesKHR
  -- ^ `pProperties`  Can be `nullPtr`. Length: pPropertyCount.
  -> m VkResult
vkGetDisplayModePropertiesKHR vk physicalDevice display pPropertyCount pProperties =
  liftIO (ffi_vkGetDisplayModePropertiesKHR (fp_vkGetDisplayModePropertiesKHR vk) physicalDevice display pPropertyCount pProperties)


foreign import ccall unsafe "dynamic" ffi_vkCreateDisplayModeKHR :: FunPtr (VkPhysicalDevice -> VkDisplayKHR -> Ptr VkDisplayModeCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkDisplayModeKHR -> IO VkResult) -> (VkPhysicalDevice -> VkDisplayKHR -> Ptr VkDisplayModeCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkDisplayModeKHR -> IO VkResult)

-- | @vkCreateDisplayModeKHR physicalDevice display pCreateInfo pAllocator pMode@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateDisplayModeKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> VkDisplayKHR
  -- ^ `display`  ExternSync=true.
  -> Ptr VkDisplayModeCreateInfoKHR
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkDisplayModeKHR
  -- ^ `pMode` 
  -> m VkResult
vkCreateDisplayModeKHR vk physicalDevice display pCreateInfo pAllocator pMode =
  liftIO (ffi_vkCreateDisplayModeKHR (fp_vkCreateDisplayModeKHR vk) physicalDevice display pCreateInfo pAllocator pMode)


foreign import ccall unsafe "dynamic" ffi_vkGetDisplayPlaneCapabilitiesKHR :: FunPtr (VkPhysicalDevice -> VkDisplayModeKHR -> Word32 -> Ptr VkDisplayPlaneCapabilitiesKHR -> IO VkResult) -> (VkPhysicalDevice -> VkDisplayModeKHR -> Word32 -> Ptr VkDisplayPlaneCapabilitiesKHR -> IO VkResult)

-- | @vkGetDisplayPlaneCapabilitiesKHR physicalDevice mode planeIndex pCapabilities@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkGetDisplayPlaneCapabilitiesKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> VkDisplayModeKHR
  -- ^ `mode`  ExternSync=true.
  -> Word32
  -- ^ `planeIndex` 
  -> Ptr VkDisplayPlaneCapabilitiesKHR
  -- ^ `pCapabilities` 
  -> m VkResult
vkGetDisplayPlaneCapabilitiesKHR vk physicalDevice mode planeIndex pCapabilities =
  liftIO (ffi_vkGetDisplayPlaneCapabilitiesKHR (fp_vkGetDisplayPlaneCapabilitiesKHR vk) physicalDevice mode planeIndex pCapabilities)


foreign import ccall unsafe "dynamic" ffi_vkCreateDisplayPlaneSurfaceKHR :: FunPtr (VkInstance -> Ptr VkDisplaySurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance -> Ptr VkDisplaySurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)

-- | @vkCreateDisplayPlaneSurfaceKHR vulkan pCreateInfo pAllocator pSurface@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateDisplayPlaneSurfaceKHR
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> Ptr VkDisplaySurfaceCreateInfoKHR
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSurfaceKHR
  -- ^ `pSurface` 
  -> m VkResult
vkCreateDisplayPlaneSurfaceKHR vk vulkan pCreateInfo pAllocator pSurface =
  liftIO (ffi_vkCreateDisplayPlaneSurfaceKHR (fp_vkCreateDisplayPlaneSurfaceKHR vk) vulkan pCreateInfo pAllocator pSurface)


foreign import ccall unsafe "dynamic" ffi_vkCreateSharedSwapchainsKHR :: FunPtr (VkDevice -> Word32 -> Ptr VkSwapchainCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult) -> (VkDevice -> Word32 -> Ptr VkSwapchainCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult)

-- | @vkCreateSharedSwapchainsKHR device swapchainCount pCreateInfos pAllocator pSwapchains@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_INCOMPATIBLE_DISPLAY_KHR,VK_ERROR_DEVICE_LOST,VK_ERROR_SURFACE_LOST_KHR q: rp: cbl: iesp:[]
vkCreateSharedSwapchainsKHR
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Word32
  -- ^ `swapchainCount` 
  -> Ptr VkSwapchainCreateInfoKHR
  -- ^ `pCreateInfos`  ExternSync=pCreateInfos[].surface,pCreateInfos[].oldSwapchain. Const. Length: swapchainCount.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSwapchainKHR
  -- ^ `pSwapchains`  Length: swapchainCount.
  -> m VkResult
vkCreateSharedSwapchainsKHR vk device swapchainCount pCreateInfos pAllocator pSwapchains =
  liftIO (ffi_vkCreateSharedSwapchainsKHR (fp_vkCreateSharedSwapchainsKHR vk) device swapchainCount pCreateInfos pAllocator pSwapchains)


foreign import ccall unsafe "dynamic" ffi_vkCreateMirSurfaceKHR :: FunPtr (VkInstance -> Ptr VkMirSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance -> Ptr VkMirSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)

-- | @vkCreateMirSurfaceKHR vulkan pCreateInfo pAllocator pSurface@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateMirSurfaceKHR
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> Ptr VkMirSurfaceCreateInfoKHR
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSurfaceKHR
  -- ^ `pSurface` 
  -> m VkResult
vkCreateMirSurfaceKHR vk vulkan pCreateInfo pAllocator pSurface =
  liftIO (ffi_vkCreateMirSurfaceKHR (fp_vkCreateMirSurfaceKHR vk) vulkan pCreateInfo pAllocator pSurface)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceMirPresentationSupportKHR :: FunPtr (VkPhysicalDevice -> Word32 -> Ptr MirConnection -> IO VkBool32) -> (VkPhysicalDevice -> Word32 -> Ptr MirConnection -> IO VkBool32)

-- | @vkGetPhysicalDeviceMirPresentationSupportKHR physicalDevice queueFamilyIndex connection@
-- 
-- == Validaty
-- * pname:queueFamilyIndex must: be less than pname:pQueueFamilyPropertyCount returned by fname:vkGetPhysicalDeviceQueueFamilyProperties for the given pname:physicalDevice
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceMirPresentationSupportKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Word32
  -- ^ `queueFamilyIndex` 
  -> Ptr MirConnection
  -- ^ `connection` 
  -> m VkBool32
vkGetPhysicalDeviceMirPresentationSupportKHR vk physicalDevice queueFamilyIndex connection =
  liftIO (ffi_vkGetPhysicalDeviceMirPresentationSupportKHR (fp_vkGetPhysicalDeviceMirPresentationSupportKHR vk) physicalDevice queueFamilyIndex connection)


foreign import ccall unsafe "dynamic" ffi_vkDestroySurfaceKHR :: FunPtr (VkInstance -> VkSurfaceKHR -> Ptr VkAllocationCallbacks -> IO ()) -> (VkInstance -> VkSurfaceKHR -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroySurfaceKHR vulkan surface pAllocator@
-- 
-- == Validaty
-- * All sname:VkSwapchainKHR objects created for pname:surface must: have been destroyed prior to destroying pname:surface
-- * If sname:VkAllocationCallbacks were provided when pname:surface was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:surface was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroySurfaceKHR
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> VkSurfaceKHR
  -- ^ `surface`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroySurfaceKHR vk vulkan surface pAllocator =
  liftIO (ffi_vkDestroySurfaceKHR (fp_vkDestroySurfaceKHR vk) vulkan surface pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceSurfaceSupportKHR :: FunPtr (VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> Ptr VkBool32 -> IO VkResult) -> (VkPhysicalDevice -> Word32 -> VkSurfaceKHR -> Ptr VkBool32 -> IO VkResult)

-- | @vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice queueFamilyIndex surface pSupported@
-- 
-- == Validaty
-- * pname:queueFamilyIndex must: be less than pname:pQueueFamilyPropertyCount returned by fname:vkGetPhysicalDeviceQueueFamilyProperties for the given pname:physicalDevice
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_SURFACE_LOST_KHR q: rp: cbl: iesp:[]
vkGetPhysicalDeviceSurfaceSupportKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Word32
  -- ^ `queueFamilyIndex` 
  -> VkSurfaceKHR
  -- ^ `surface` 
  -> Ptr VkBool32
  -- ^ `pSupported` 
  -> m VkResult
vkGetPhysicalDeviceSurfaceSupportKHR vk physicalDevice queueFamilyIndex surface pSupported =
  liftIO (ffi_vkGetPhysicalDeviceSurfaceSupportKHR (fp_vkGetPhysicalDeviceSurfaceSupportKHR vk) physicalDevice queueFamilyIndex surface pSupported)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceSurfaceCapabilitiesKHR :: FunPtr (VkPhysicalDevice -> VkSurfaceKHR -> Ptr VkSurfaceCapabilitiesKHR -> IO VkResult) -> (VkPhysicalDevice -> VkSurfaceKHR -> Ptr VkSurfaceCapabilitiesKHR -> IO VkResult)

-- | @vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface pSurfaceCapabilities@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_SURFACE_LOST_KHR q: rp: cbl: iesp:[]
vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> VkSurfaceKHR
  -- ^ `surface` 
  -> Ptr VkSurfaceCapabilitiesKHR
  -- ^ `pSurfaceCapabilities` 
  -> m VkResult
vkGetPhysicalDeviceSurfaceCapabilitiesKHR vk physicalDevice surface pSurfaceCapabilities =
  liftIO (ffi_vkGetPhysicalDeviceSurfaceCapabilitiesKHR (fp_vkGetPhysicalDeviceSurfaceCapabilitiesKHR vk) physicalDevice surface pSurfaceCapabilities)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceSurfaceFormatsKHR :: FunPtr (VkPhysicalDevice -> VkSurfaceKHR -> Ptr Word32 -> Ptr VkSurfaceFormatKHR -> IO VkResult) -> (VkPhysicalDevice -> VkSurfaceKHR -> Ptr Word32 -> Ptr VkSurfaceFormatKHR -> IO VkResult)

-- | @vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice surface pSurfaceFormatCount pSurfaceFormats@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_SURFACE_LOST_KHR q: rp: cbl: iesp:[]
vkGetPhysicalDeviceSurfaceFormatsKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> VkSurfaceKHR
  -- ^ `surface` 
  -> Ptr Word32
  -- ^ `pSurfaceFormatCount`  Optional=false,true.
  -> Ptr VkSurfaceFormatKHR
  -- ^ `pSurfaceFormats`  Can be `nullPtr`. Length: pSurfaceFormatCount.
  -> m VkResult
vkGetPhysicalDeviceSurfaceFormatsKHR vk physicalDevice surface pSurfaceFormatCount pSurfaceFormats =
  liftIO (ffi_vkGetPhysicalDeviceSurfaceFormatsKHR (fp_vkGetPhysicalDeviceSurfaceFormatsKHR vk) physicalDevice surface pSurfaceFormatCount pSurfaceFormats)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceSurfacePresentModesKHR :: FunPtr (VkPhysicalDevice -> VkSurfaceKHR -> Ptr Word32 -> Ptr VkPresentModeKHR -> IO VkResult) -> (VkPhysicalDevice -> VkSurfaceKHR -> Ptr Word32 -> Ptr VkPresentModeKHR -> IO VkResult)

-- | @vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice surface pPresentModeCount pPresentModes@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_SURFACE_LOST_KHR q: rp: cbl: iesp:[]
vkGetPhysicalDeviceSurfacePresentModesKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> VkSurfaceKHR
  -- ^ `surface` 
  -> Ptr Word32
  -- ^ `pPresentModeCount`  Optional=false,true.
  -> Ptr VkPresentModeKHR
  -- ^ `pPresentModes`  Can be `nullPtr`. Length: pPresentModeCount.
  -> m VkResult
vkGetPhysicalDeviceSurfacePresentModesKHR vk physicalDevice surface pPresentModeCount pPresentModes =
  liftIO (ffi_vkGetPhysicalDeviceSurfacePresentModesKHR (fp_vkGetPhysicalDeviceSurfacePresentModesKHR vk) physicalDevice surface pPresentModeCount pPresentModes)


foreign import ccall unsafe "dynamic" ffi_vkCreateSwapchainKHR :: FunPtr (VkDevice -> Ptr VkSwapchainCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult) -> (VkDevice -> Ptr VkSwapchainCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSwapchainKHR -> IO VkResult)

-- | @vkCreateSwapchainKHR device pCreateInfo pAllocator pSwapchain@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST,VK_ERROR_SURFACE_LOST_KHR,VK_ERROR_NATIVE_WINDOW_IN_USE_KHR q: rp: cbl: iesp:[]
vkCreateSwapchainKHR
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkSwapchainCreateInfoKHR
  -- ^ `pCreateInfo`  ExternSync=pCreateInfo.surface,pCreateInfo.oldSwapchain. Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSwapchainKHR
  -- ^ `pSwapchain` 
  -> m VkResult
vkCreateSwapchainKHR vk device pCreateInfo pAllocator pSwapchain =
  liftIO (ffi_vkCreateSwapchainKHR (fp_vkCreateSwapchainKHR vk) device pCreateInfo pAllocator pSwapchain)


foreign import ccall unsafe "dynamic" ffi_vkDestroySwapchainKHR :: FunPtr (VkDevice -> VkSwapchainKHR -> Ptr VkAllocationCallbacks -> IO ()) -> (VkDevice -> VkSwapchainKHR -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroySwapchainKHR device swapchain pAllocator@
-- 
-- == Validaty
-- * All uses of presentable images acquired from pname:swapchain must: have completed execution
-- * If sname:VkAllocationCallbacks were provided when pname:swapchain was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:swapchain was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroySwapchainKHR
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkSwapchainKHR
  -- ^ `swapchain`  Can be `nullPtr`. ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroySwapchainKHR vk device swapchain pAllocator =
  liftIO (ffi_vkDestroySwapchainKHR (fp_vkDestroySwapchainKHR vk) device swapchain pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkGetSwapchainImagesKHR :: FunPtr (VkDevice -> VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO VkResult) -> (VkDevice -> VkSwapchainKHR -> Ptr Word32 -> Ptr VkImage -> IO VkResult)

-- | @vkGetSwapchainImagesKHR device swapchain pSwapchainImageCount pSwapchainImages@
-- 
-- 
-- s:VK_SUCCESS,VK_INCOMPLETE e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkGetSwapchainImagesKHR
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkSwapchainKHR
  -- ^ `swapchain` 
  -> Ptr Word32
  -- ^ `pSwapchainImageCount`  Optional=false,true.
  -> Ptr VkImage
  -- ^ `pSwapchainImages`  Can be `nullPtr`. Length: pSwapchainImageCount.
  -> m VkResult
vkGetSwapchainImagesKHR vk device swapchain pSwapchainImageCount pSwapchainImages =
  liftIO (ffi_vkGetSwapchainImagesKHR (fp_vkGetSwapchainImagesKHR vk) device swapchain pSwapchainImageCount pSwapchainImages)


foreign import ccall unsafe "dynamic" ffi_vkAcquireNextImageKHR :: FunPtr (VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO VkResult) -> (VkDevice -> VkSwapchainKHR -> Word64 -> VkSemaphore -> VkFence -> Ptr Word32 -> IO VkResult)

-- | @vkAcquireNextImageKHR device swapchain timeout semaphore fence pImageIndex@
-- 
-- == Validaty
-- * If pname:semaphore is not sname:VK_NULL_HANDLE it must: be unsignalled
-- * If pname:fence is not sname:VK_NULL_HANDLE it must: be unsignalled and mustnot: be associated with any other queue command that has not yet completed execution on that queue
-- 
-- s:VK_SUCCESS,VK_TIMEOUT,VK_NOT_READY,VK_SUBOPTIMAL_KHR e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST,VK_ERROR_OUT_OF_DATE_KHR,VK_ERROR_SURFACE_LOST_KHR q: rp: cbl: iesp:[]
vkAcquireNextImageKHR
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> VkSwapchainKHR
  -- ^ `swapchain`  ExternSync=true.
  -> Word64
  -- ^ `timeout` 
  -> VkSemaphore
  -- ^ `semaphore`  Can be `nullPtr`. ExternSync=true.
  -> VkFence
  -- ^ `fence`  Can be `nullPtr`. ExternSync=true.
  -> Ptr Word32
  -- ^ `pImageIndex` 
  -> m VkResult
vkAcquireNextImageKHR vk device swapchain timeout semaphore fence pImageIndex =
  liftIO (ffi_vkAcquireNextImageKHR (fp_vkAcquireNextImageKHR vk) device swapchain timeout semaphore fence pImageIndex)


foreign import ccall unsafe "dynamic" ffi_vkQueuePresentKHR :: FunPtr (VkQueue -> Ptr VkPresentInfoKHR -> IO VkResult) -> (VkQueue -> Ptr VkPresentInfoKHR -> IO VkResult)

-- | @vkQueuePresentKHR queue pPresentInfo@
-- 
-- == Validaty
-- * Any given element of pname:pSwapchains member of pname:pPresentInfo must: be a swapchain that is created for a surface for which presentation is supported from pname:queue as determined using a call to fname:vkGetPhysicalDeviceSurfaceSupportKHR
-- 
-- s:VK_SUCCESS,VK_SUBOPTIMAL_KHR e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY,VK_ERROR_DEVICE_LOST,VK_ERROR_OUT_OF_DATE_KHR,VK_ERROR_SURFACE_LOST_KHR q: rp: cbl: iesp:[]
vkQueuePresentKHR
  :: MonadIO m => Vulkan
  -> VkQueue
  -- ^ `queue`  ExternSync=true.
  -> Ptr VkPresentInfoKHR
  -- ^ `pPresentInfo`  ExternSync=pPresentInfo.pWaitSemaphores[],pPresentInfo.pSwapchains[]. Const.
  -> m VkResult
vkQueuePresentKHR vk queue pPresentInfo =
  liftIO (ffi_vkQueuePresentKHR (fp_vkQueuePresentKHR vk) queue pPresentInfo)


foreign import ccall unsafe "dynamic" ffi_vkCreateWaylandSurfaceKHR :: FunPtr (VkInstance -> Ptr VkWaylandSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance -> Ptr VkWaylandSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)

-- | @vkCreateWaylandSurfaceKHR vulkan pCreateInfo pAllocator pSurface@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateWaylandSurfaceKHR
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> Ptr VkWaylandSurfaceCreateInfoKHR
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSurfaceKHR
  -- ^ `pSurface` 
  -> m VkResult
vkCreateWaylandSurfaceKHR vk vulkan pCreateInfo pAllocator pSurface =
  liftIO (ffi_vkCreateWaylandSurfaceKHR (fp_vkCreateWaylandSurfaceKHR vk) vulkan pCreateInfo pAllocator pSurface)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceWaylandPresentationSupportKHR :: FunPtr (VkPhysicalDevice -> Word32 -> Ptr WlDisplay -> IO VkBool32) -> (VkPhysicalDevice -> Word32 -> Ptr WlDisplay -> IO VkBool32)

-- | @vkGetPhysicalDeviceWaylandPresentationSupportKHR physicalDevice queueFamilyIndex display@
-- 
-- == Validaty
-- * pname:queueFamilyIndex must: be less than pname:pQueueFamilyPropertyCount returned by fname:vkGetPhysicalDeviceQueueFamilyProperties for the given pname:physicalDevice
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceWaylandPresentationSupportKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Word32
  -- ^ `queueFamilyIndex` 
  -> Ptr WlDisplay
  -- ^ `display` 
  -> m VkBool32
vkGetPhysicalDeviceWaylandPresentationSupportKHR vk physicalDevice queueFamilyIndex display =
  liftIO (ffi_vkGetPhysicalDeviceWaylandPresentationSupportKHR (fp_vkGetPhysicalDeviceWaylandPresentationSupportKHR vk) physicalDevice queueFamilyIndex display)


foreign import ccall unsafe "dynamic" ffi_vkCreateWin32SurfaceKHR :: FunPtr (VkInstance -> Ptr VkWin32SurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance -> Ptr VkWin32SurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)

-- | @vkCreateWin32SurfaceKHR vulkan pCreateInfo pAllocator pSurface@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateWin32SurfaceKHR
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> Ptr VkWin32SurfaceCreateInfoKHR
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSurfaceKHR
  -- ^ `pSurface` 
  -> m VkResult
vkCreateWin32SurfaceKHR vk vulkan pCreateInfo pAllocator pSurface =
  liftIO (ffi_vkCreateWin32SurfaceKHR (fp_vkCreateWin32SurfaceKHR vk) vulkan pCreateInfo pAllocator pSurface)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceWin32PresentationSupportKHR :: FunPtr (VkPhysicalDevice -> Word32 -> IO VkBool32) -> (VkPhysicalDevice -> Word32 -> IO VkBool32)

-- | @vkGetPhysicalDeviceWin32PresentationSupportKHR physicalDevice queueFamilyIndex@
-- 
-- == Validaty
-- * pname:queueFamilyIndex must: be less than pname:pQueueFamilyPropertyCount returned by fname:vkGetPhysicalDeviceQueueFamilyProperties for the given pname:physicalDevice
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceWin32PresentationSupportKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Word32
  -- ^ `queueFamilyIndex` 
  -> m VkBool32
vkGetPhysicalDeviceWin32PresentationSupportKHR vk physicalDevice queueFamilyIndex =
  liftIO (ffi_vkGetPhysicalDeviceWin32PresentationSupportKHR (fp_vkGetPhysicalDeviceWin32PresentationSupportKHR vk) physicalDevice queueFamilyIndex)


foreign import ccall unsafe "dynamic" ffi_vkCreateXlibSurfaceKHR :: FunPtr (VkInstance -> Ptr VkXlibSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance -> Ptr VkXlibSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)

-- | @vkCreateXlibSurfaceKHR vulkan pCreateInfo pAllocator pSurface@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateXlibSurfaceKHR
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> Ptr VkXlibSurfaceCreateInfoKHR
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSurfaceKHR
  -- ^ `pSurface` 
  -> m VkResult
vkCreateXlibSurfaceKHR vk vulkan pCreateInfo pAllocator pSurface =
  liftIO (ffi_vkCreateXlibSurfaceKHR (fp_vkCreateXlibSurfaceKHR vk) vulkan pCreateInfo pAllocator pSurface)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceXlibPresentationSupportKHR :: FunPtr (VkPhysicalDevice -> Word32 -> Ptr Display -> VisualID -> IO VkBool32) -> (VkPhysicalDevice -> Word32 -> Ptr Display -> VisualID -> IO VkBool32)

-- | @vkGetPhysicalDeviceXlibPresentationSupportKHR physicalDevice queueFamilyIndex dpy visualID@
-- 
-- == Validaty
-- * pname:queueFamilyIndex must: be less than pname:pQueueFamilyPropertyCount returned by fname:vkGetPhysicalDeviceQueueFamilyProperties for the given pname:physicalDevice
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceXlibPresentationSupportKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Word32
  -- ^ `queueFamilyIndex` 
  -> Ptr Display
  -- ^ `dpy` 
  -> VisualID
  -- ^ `visualID` 
  -> m VkBool32
vkGetPhysicalDeviceXlibPresentationSupportKHR vk physicalDevice queueFamilyIndex dpy visualID =
  liftIO (ffi_vkGetPhysicalDeviceXlibPresentationSupportKHR (fp_vkGetPhysicalDeviceXlibPresentationSupportKHR vk) physicalDevice queueFamilyIndex dpy visualID)


foreign import ccall unsafe "dynamic" ffi_vkCreateXcbSurfaceKHR :: FunPtr (VkInstance -> Ptr VkXcbSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult) -> (VkInstance -> Ptr VkXcbSurfaceCreateInfoKHR -> Ptr VkAllocationCallbacks -> Ptr VkSurfaceKHR -> IO VkResult)

-- | @vkCreateXcbSurfaceKHR vulkan pCreateInfo pAllocator pSurface@
-- 
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkCreateXcbSurfaceKHR
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> Ptr VkXcbSurfaceCreateInfoKHR
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkSurfaceKHR
  -- ^ `pSurface` 
  -> m VkResult
vkCreateXcbSurfaceKHR vk vulkan pCreateInfo pAllocator pSurface =
  liftIO (ffi_vkCreateXcbSurfaceKHR (fp_vkCreateXcbSurfaceKHR vk) vulkan pCreateInfo pAllocator pSurface)


foreign import ccall unsafe "dynamic" ffi_vkGetPhysicalDeviceXcbPresentationSupportKHR :: FunPtr (VkPhysicalDevice -> Word32 -> Ptr XcbConnection -> XcbVisualId -> IO VkBool32) -> (VkPhysicalDevice -> Word32 -> Ptr XcbConnection -> XcbVisualId -> IO VkBool32)

-- | @vkGetPhysicalDeviceXcbPresentationSupportKHR physicalDevice queueFamilyIndex connection visual_id@
-- 
-- == Validaty
-- * pname:queueFamilyIndex must: be less than pname:pQueueFamilyPropertyCount returned by fname:vkGetPhysicalDeviceQueueFamilyProperties for the given pname:physicalDevice
-- 
-- s: e: q: rp: cbl: iesp:[]
vkGetPhysicalDeviceXcbPresentationSupportKHR
  :: MonadIO m => VulkanSetup
  -> VkPhysicalDevice
  -- ^ `physicalDevice` 
  -> Word32
  -- ^ `queueFamilyIndex` 
  -> Ptr XcbConnection
  -- ^ `connection` 
  -> XcbVisualId
  -- ^ `visual_id` 
  -> m VkBool32
vkGetPhysicalDeviceXcbPresentationSupportKHR vk physicalDevice queueFamilyIndex connection visual_id =
  liftIO (ffi_vkGetPhysicalDeviceXcbPresentationSupportKHR (fp_vkGetPhysicalDeviceXcbPresentationSupportKHR vk) physicalDevice queueFamilyIndex connection visual_id)


foreign import ccall unsafe "dynamic" ffi_vkCreateDebugReportCallbackEXT :: FunPtr (VkInstance -> Ptr VkDebugReportCallbackCreateInfoEXT -> Ptr VkAllocationCallbacks -> Ptr VkDebugReportCallbackEXT -> IO VkResult) -> (VkInstance -> Ptr VkDebugReportCallbackCreateInfoEXT -> Ptr VkAllocationCallbacks -> Ptr VkDebugReportCallbackEXT -> IO VkResult)

-- | @vkCreateDebugReportCallbackEXT vulkan pCreateInfo pAllocator pCallback@
-- 
-- 
-- s:VK_SUCCESS e: q: rp: cbl: iesp:[]
vkCreateDebugReportCallbackEXT
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> Ptr VkDebugReportCallbackCreateInfoEXT
  -- ^ `pCreateInfo`  Const.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> Ptr VkDebugReportCallbackEXT
  -- ^ `pCallback` 
  -> m VkResult
vkCreateDebugReportCallbackEXT vk vulkan pCreateInfo pAllocator pCallback =
  liftIO (ffi_vkCreateDebugReportCallbackEXT (fp_vkCreateDebugReportCallbackEXT vk) vulkan pCreateInfo pAllocator pCallback)


foreign import ccall unsafe "dynamic" ffi_vkDestroyDebugReportCallbackEXT :: FunPtr (VkInstance -> VkDebugReportCallbackEXT -> Ptr VkAllocationCallbacks -> IO ()) -> (VkInstance -> VkDebugReportCallbackEXT -> Ptr VkAllocationCallbacks -> IO ())

-- | @vkDestroyDebugReportCallbackEXT vulkan callback pAllocator@
-- 
-- == Validaty
-- * If sname:VkAllocationCallbacks were provided when pname:instance was created, a compatible set of callbacks must: be provided here
-- * If no sname:VkAllocationCallbacks were provided when pname:instance was created, pname:pAllocator must: be `NULL`
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDestroyDebugReportCallbackEXT
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> VkDebugReportCallbackEXT
  -- ^ `callback`  ExternSync=true.
  -> Ptr VkAllocationCallbacks
  -- ^ `pAllocator`  Can be `nullPtr`. Const.
  -> m ()
vkDestroyDebugReportCallbackEXT vk vulkan callback pAllocator =
  liftIO (ffi_vkDestroyDebugReportCallbackEXT (fp_vkDestroyDebugReportCallbackEXT vk) vulkan callback pAllocator)


foreign import ccall unsafe "dynamic" ffi_vkDebugReportMessageEXT :: FunPtr (VkInstance -> VkDebugReportFlagsEXT -> VkDebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ()) -> (VkInstance -> VkDebugReportFlagsEXT -> VkDebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ())

-- | @vkDebugReportMessageEXT vulkan flags objectType object location messageCode pLayerPrefix pMessage@
-- 
-- == Validaty
-- * pname:instance must: be a valid sname:VkInstance handle
-- * pname:flags must: be a combination of one or more of sname:VkDebugReportFlagBitsEXT
-- * pname:objType must: be one of sname:VkDebugReportObjectTypeEXT, ename:VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT if pname:object is `NULL`
-- * pname:object may: be a {apiname} object
-- * pname:pLayerPrefix must: be a `NULL` terminated string.
-- * pname:pMsg must: be a `NULL` terminated string.
-- 
-- s: e: q: rp: cbl: iesp:[]
vkDebugReportMessageEXT
  :: MonadIO m => VulkanSetup
  -> VkInstance
  -- ^ `vulkan` 
  -> VkDebugReportFlagsEXT
  -- ^ `flags` 
  -> VkDebugReportObjectTypeEXT
  -- ^ `objectType` 
  -> Word64
  -- ^ `object` 
  -> CSize
  -- ^ `location` 
  -> Int32
  -- ^ `messageCode` 
  -> Ptr CChar
  -- ^ `pLayerPrefix`  Const.
  -> Ptr CChar
  -- ^ `pMessage`  Const.
  -> m ()
vkDebugReportMessageEXT vk vulkan flags objectType object location messageCode pLayerPrefix pMessage =
  liftIO (ffi_vkDebugReportMessageEXT (fp_vkDebugReportMessageEXT vk) vulkan flags objectType object location messageCode pLayerPrefix pMessage)


foreign import ccall unsafe "dynamic" ffi_vkDebugMarkerSetObjectNameEXT :: FunPtr (VkDevice -> Ptr VkDebugMarkerObjectNameInfoEXT -> IO VkResult) -> (VkDevice -> Ptr VkDebugMarkerObjectNameInfoEXT -> IO VkResult)

-- | @vkDebugMarkerSetObjectNameEXT device pNameInfo@
-- 
-- == Validaty
-- * pname:pNameInfo.object must: be a Vulkan object
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkDebugMarkerSetObjectNameEXT
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkDebugMarkerObjectNameInfoEXT
  -- ^ `pNameInfo`  ExternSync=pNameInfo.object.
  -> m VkResult
vkDebugMarkerSetObjectNameEXT vk device pNameInfo =
  liftIO (ffi_vkDebugMarkerSetObjectNameEXT (fp_vkDebugMarkerSetObjectNameEXT vk) device pNameInfo)


foreign import ccall unsafe "dynamic" ffi_vkDebugMarkerSetObjectTagEXT :: FunPtr (VkDevice -> Ptr VkDebugMarkerObjectTagInfoEXT -> IO VkResult) -> (VkDevice -> Ptr VkDebugMarkerObjectTagInfoEXT -> IO VkResult)

-- | @vkDebugMarkerSetObjectTagEXT device pTagInfo@
-- 
-- == Validaty
-- * pname:pTagInfo.object must: be a Vulkan object
-- * pname:pTagInfo.tagName mustnot: be `0`
-- 
-- s:VK_SUCCESS e:VK_ERROR_OUT_OF_HOST_MEMORY,VK_ERROR_OUT_OF_DEVICE_MEMORY q: rp: cbl: iesp:[]
vkDebugMarkerSetObjectTagEXT
  :: MonadIO m => Vulkan
  -> VkDevice
  -- ^ `device` 
  -> Ptr VkDebugMarkerObjectTagInfoEXT
  -- ^ `pTagInfo`  ExternSync=pTagInfo.object.
  -> m VkResult
vkDebugMarkerSetObjectTagEXT vk device pTagInfo =
  liftIO (ffi_vkDebugMarkerSetObjectTagEXT (fp_vkDebugMarkerSetObjectTagEXT vk) device pTagInfo)


foreign import ccall unsafe "dynamic" ffi_vkCmdDebugMarkerBeginEXT :: FunPtr (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()) -> (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ())

-- | @vkCmdDebugMarkerBeginEXT commandBuffer pMarkerInfo@
-- 
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdDebugMarkerBeginEXT
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer` 
  -> Ptr VkDebugMarkerMarkerInfoEXT
  -- ^ `pMarkerInfo` 
  -> m ()
vkCmdDebugMarkerBeginEXT vk commandBuffer pMarkerInfo =
  liftIO (ffi_vkCmdDebugMarkerBeginEXT (fp_vkCmdDebugMarkerBeginEXT vk) commandBuffer pMarkerInfo)


foreign import ccall unsafe "dynamic" ffi_vkCmdDebugMarkerEndEXT :: FunPtr (VkCommandBuffer -> IO ()) -> (VkCommandBuffer -> IO ())

-- | @vkCmdDebugMarkerEndEXT commandBuffer@
-- 
-- == Validaty
-- * There must: be an outstanding flink:vkCmdDebugMarkerBeginEXT command prior to the fname:vkCmdDebugMarkerEndEXT on the queue that pname:commandBuffer is submitted to.
-- * If the matching flink:vkCmdDebugMarkerBeginEXT command was in a secondary command buffer, the fname:vkCmdDebugMarkerEndEXT must be in the same pname:commandBuffer.
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdDebugMarkerEndEXT
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer` 
  -> m ()
vkCmdDebugMarkerEndEXT vk commandBuffer =
  liftIO (ffi_vkCmdDebugMarkerEndEXT (fp_vkCmdDebugMarkerEndEXT vk) commandBuffer)


foreign import ccall unsafe "dynamic" ffi_vkCmdDebugMarkerInsertEXT :: FunPtr (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ()) -> (VkCommandBuffer -> Ptr VkDebugMarkerMarkerInfoEXT -> IO ())

-- | @vkCmdDebugMarkerInsertEXT commandBuffer pMarkerInfo@
-- 
-- 
-- s: e: q: rp:both cbl:primary,secondary iesp:[]
vkCmdDebugMarkerInsertEXT
  :: MonadIO m => Vulkan
  -> VkCommandBuffer
  -- ^ `commandBuffer` 
  -> Ptr VkDebugMarkerMarkerInfoEXT
  -- ^ `pMarkerInfo` 
  -> m ()
vkCmdDebugMarkerInsertEXT vk commandBuffer pMarkerInfo =
  liftIO (ffi_vkCmdDebugMarkerInsertEXT (fp_vkCmdDebugMarkerInsertEXT vk) commandBuffer pMarkerInfo)

-- | 
pattern VK_KHR_SURFACE_SPEC_VERSION = 25
-- | 
pattern VK_KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"
-- | 
pattern VK_ERROR_SURFACE_LOST_KHR = VkResult (-1000000000)
-- | 
pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR = VkResult (-1000000001)
-- | 
pattern VK_KHR_SWAPCHAIN_SPEC_VERSION = 68
-- | 
pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"
-- | 
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR = VkStructureType 1000001000
-- | 
pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR = VkStructureType 1000001001
-- | 
pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR = VkImageLayout 1000001002
-- | 
pattern VK_SUBOPTIMAL_KHR = VkResult 1000001003
-- | 
pattern VK_ERROR_OUT_OF_DATE_KHR = VkResult (-1000001004)
-- | 
pattern VK_KHR_DISPLAY_SPEC_VERSION = 21
-- | 
pattern VK_KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"
-- | 
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR = VkStructureType 1000002000
-- | 
pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR = VkStructureType 1000002001
-- | 
pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 9
-- | 
pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_display_swapchain"
-- | 
pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR = VkStructureType 1000003000
-- | 
pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR = VkResult (-1000003001)
-- | 
pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION = 6
-- | 
pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME = "VK_KHR_xlib_surface"
-- | 
pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR = VkStructureType 1000004000
-- | 
pattern VK_KHR_XCB_SURFACE_SPEC_VERSION = 6
-- | 
pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME = "VK_KHR_xcb_surface"
-- | 
pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR = VkStructureType 1000005000
-- | 
pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION = 5
-- | 
pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME = "VK_KHR_wayland_surface"
-- | 
pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = VkStructureType 1000006000
-- | 
pattern VK_KHR_MIR_SURFACE_SPEC_VERSION = 4
-- | 
pattern VK_KHR_MIR_SURFACE_EXTENSION_NAME = "VK_KHR_mir_surface"
-- | 
pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR = VkStructureType 1000007000
-- | 
pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION = 6
-- | 
pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME = "VK_KHR_android_surface"
-- | 
pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR = VkStructureType 1000008000
-- | 
pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION = 5
-- | 
pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME = "VK_KHR_win32_surface"
-- | 
pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR = VkStructureType 1000009000
-- | 
pattern VK_ANDROID_NATIVE_BUFFER_SPEC_VERSION = 4
-- | 
pattern VK_ANDROID_NATIVE_BUFFER_NUMBER = 11
-- | 
pattern VK_ANDROID_NATIVE_BUFFER_NAME = "VK_ANDROID_native_buffer"
-- | 
pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION = 2
-- | 
pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"
-- | 
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT = VkStructureType 1000011000
-- | 
pattern VK_ERROR_VALIDATION_FAILED_EXT = VkResult (-1000011001)
-- | 
pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT = VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
-- | 
pattern VK_NV_GLSL_SHADER_SPEC_VERSION = 1
-- | 
pattern VK_NV_GLSL_SHADER_EXTENSION_NAME = "VK_NV_glsl_shader"
-- | 
pattern VK_ERROR_INVALID_SHADER_NV = VkResult (-1000012000)
-- | 
pattern VK_NV_EXTENSION_1_SPEC_VERSION = 0
-- | 
pattern VK_NV_EXTENSION_1_EXTENSION_NAME = "VK_NV_extension_1"
-- | 
pattern VK_NV_EXTENSION_1_ERROR = VkResult (-1000013000)
-- | 
pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 1
-- | 
pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME = "VK_KHR_sampler_mirror_clamp_to_edge"
-- | Note that this defines what was previously a core enum, and so uses the 'value' attribute rather than 'offset', and doesn't have a suffix. This is a special case, and should not be repeated
pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = VkSamplerAddressMode 4
-- | 
pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION = 1
-- | 
pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME = "VK_IMG_filter_cubic"
-- | 
pattern VK_FILTER_CUBIC_IMG = VkFilter 1000015000
-- | Format can be filtered with VK_FILTER_CUBIC_IMG when being sampled
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG = VkFormatFeatureFlagBits 8192
-- | 
pattern VK_AMD_EXTENSION_1_SPEC_VERSION = 0
-- | 
pattern VK_AMD_EXTENSION_1_EXTENSION_NAME = "VK_AMD_extension_1"
-- | 
pattern VK_AMD_EXTENSION_2_SPEC_VERSION = 0
-- | 
pattern VK_AMD_EXTENSION_2_EXTENSION_NAME = "VK_AMD_extension_2"
-- | 
pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1
-- | 
pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME = "VK_AMD_rasterization_order"
-- | 
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD = VkStructureType 1000018000
-- | 
pattern VK_AMD_EXTENSION_4_SPEC_VERSION = 0
-- | 
pattern VK_AMD_EXTENSION_4_EXTENSION_NAME = "VK_AMD_extension_4"
-- | 
pattern VK_AMD_EXTENSION_5_SPEC_VERSION = 0
-- | 
pattern VK_AMD_EXTENSION_5_EXTENSION_NAME = "VK_AMD_extension_5"
-- | 
pattern VK_AMD_EXTENSION_6_SPEC_VERSION = 0
-- | 
pattern VK_AMD_EXTENSION_6_EXTENSION_NAME = "VK_AMD_extension_6"
-- | 
pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION = 3
-- | 
pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"
-- | 
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT = VkStructureType 1000022000
-- | 
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT = VkStructureType 1000022001
-- | 
pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT = VkStructureType 1000022002
-- End of File
